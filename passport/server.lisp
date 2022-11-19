(uiop:define-package #:passport/server
  (:use #:cl
        #:common/utils)
  (:import-from #:rating/client
                #:make-rating)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:passport/user
                #:adminp
                #:moderatedp
                #:is-blocked-p
                #:user-profession-id
                #:user-profession
                #:user-with-rating
                #:issue-token-for
                #:get-user-by
                #:is-email-available-p
                #:get-next-user-id
                #:user-email
                #:user-password-hash
                #:user)
  (:import-from #:common/token
                #:get-jwt-secret)
  (:import-from #:openrpc-server/api
                #:define-api)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:common/server)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:common/session
                #:with-session)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:common/rpc
                #:define-update-method)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:sxql
                #:limit
                #:order-by)
  (:import-from #:mito
                #:find-dao)
  (:import-from #:platform/client
                #:make-platform)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:common/permissions
                #:get-password-hash)
  (:import-from #:common/avatar
                #:get-avatar-url-for)
  (:export
   #:start-me
   #:stop-me))
(in-package #:passport/server)


(define-api (passport-api :title "Passport API"))


(define-rpc-method (passport-api login) (email password)
  (:param email string)
  (:param password string)
  (:result string)
  (with-connection ()
    (let* ((hash (get-password-hash password))
           (user (get-user-by email))
           (user-hash (when user
                        (user-password-hash user))))
      (cond
        ((equal user-hash hash)
         (issue-token-for user))
        (t
         (openrpc-server:return-error "Неправильный email или пароль." :code 1))))))


(define-rpc-method (passport-api signup) (email password fio)
  (:param email string)
  (:param password string)
  (:param fio string)
  (:result string)
  (log:info "Signup")
  (with-connection ()
    (cond
      ((is-email-available-p email)
       (let ((user (mito:create-dao 'user
                                    :id (get-next-user-id)
                                    :email email
                                    :fio fio
                                    :avatar-url (get-avatar-url-for email)
                                    :password-hash (get-password-hash password))))
         (issue-token-for user)))
      (t
       (return-error (format nil "Email ~A уже занят."
                             email)
                     :code 2)))))


(define-rpc-method (passport-api my-profile) (&key additional-fields)
  (:summary "Отдаёт профиль текущего залогиненого пользователя.")
  (:description "В additional-fields можно передать \"projects\", чтобы в поле \"projects\" подтянулись проекты пользователя.")
  (:param additional-fields (list-of string)
          "В этом списке строк можно указывать только \"projects\".")
  (:result user)
  (with-connection ()
    (with-session (user-id)
      (let ((user (find-dao 'user
                            :id user-id)))
        (enrich-user user additional-fields)))))


(define-rpc-method (passport-api my-roles) ()
  (:summary "Отдаёт список строк с ролями текущего залогинового пользователя.")
  (:result (list-of string))
  (with-session ((roles))
    (map 'vector #'string-downcase roles)))


(define-update-method (passport-api update-profile user)
                      (fio
                       birthday
                       gender
                       phone)
  (with-session (user-id)
    (find-dao 'user
              :id user-id)))


(defun enrich-users (users additional-fields)
  (declare (ignore additional-fields))
  users)


(defun enrich-user (user additional-fields)
  (enrich-users (list user) additional-fields)
  user)


(define-rpc-method (passport-api get-profile) (id &key additional-fields)
  (:param id integer "ID пользователя")
  (:param additional-fields (list-of string)
          "Опциональный список полей, которые нужно заполнить для пользователя.
           Пока поддерживается только \"projects\".")
  (:result user)
  (with-connection ()
    (let ((user (mito:find-dao 'user
                               :id id)))
      (cond
        (user
         (enrich-user user additional-fields))
        (t
         (return-error (fmt "Пользователь с id = ~A не найден."
                            id)
                       :code 4))))))


(define-rpc-method (passport-api profiles-on-moderation) (&key (limit 10))
  (:param limit integer)
  (:result (list-of user))

  (with-connection ()
    (mito:select-dao 'user
      (where (:= :moderated 0))
      (order-by :created-at)
      (limit limit))))


(define-rpc-method (passport-api block-user) (id-to-block)
  (:summary "Блокирует пользователя.")
  (:param id-to-block integer "ID пользователя которого надо заблочить.")
  (:result user)

  (with-session ((roles))
   (unless (member "admin" roles :test #'string-equal)
     (return-error "Только администратор может блокировать пользователей."))
    
   (with-connection ()
     (let ((user (mito:find-dao 'user :id id-to-block)))
       (unless user
         (return-error (fmt "Пользователь с ID ~A не найден."
                            id-to-block)))
       (log:warn "Blocking user with id" id-to-block)
       (setf (is-blocked-p user) t)
       (setf (moderatedp user) t)
       (mito:save-dao user)
       (values user)))))


(define-rpc-method (passport-api admit-user) (id-to-admit)
  (:summary "Разрешает пользователю пользоваться сервисом.")
  (:param id-to-admit integer "ID пользователя которого надо впустить.")
  (:result user)

  (with-session ((roles))
   (unless (member "admin" roles :test #'string-equal)
     (return-error "Только администратор может впускать пользователей."))
    
   (with-connection ()
     (let ((user (mito:find-dao 'user :id id-to-admit)))
       (unless user
         (return-error (fmt "Пользователь с ID ~A не найден."
                            id-to-admit)))
       (log:warn "Admitting user with id" id-to-admit)
       (setf (is-blocked-p user) nil
             (moderatedp user) t)
       (mito:save-dao user)
       (values user)))))


(define-rpc-method (passport-api make-user-admin) (future-admin-id)
  (:summary "Впускает пользователя и делает его админом.")
  (:param future-admin-id integer "ID пользователя которого надо впустить и сделать админом.")
  (:result user)

  (with-session ((roles))
    (unless (member "admin" roles :test #'string-equal)
      (return-error "Только администратор может впускать пользователей."))
    
    (with-connection ()
      (let ((user (mito:find-dao 'user :id future-admin-id)))
        (unless user
          (return-error (fmt "Пользователь с ID ~A не найден."
                             future-admin-id)))
        (log:warn "Making user with id" future-admin-id "admin")
        (setf (is-blocked-p user) nil
              (adminp user) t
              (moderatedp user) t)
        (mito:save-dao user)
        (values user)))))


(define-rpc-method (passport-api is-client) ()
  (:summary "Проверяет, что текущий пользователь является действительным клиентом банка.")
  (:description "Для прода тут надо прикрутить кэш.")
  (:result boolean)

  (with-session ((user-id))
    (with-connection ()
      (let ((user (mito:find-dao 'user :id user-id)))
        (cond
          (user (and
                 (moderatedp user)
                 (not (is-blocked-p user))))
          (t
           nil))))))


(defun start-me ()
  (common/server::start passport-api 8000))

(defun stop-me ()
  (common/server::stop 8000))
