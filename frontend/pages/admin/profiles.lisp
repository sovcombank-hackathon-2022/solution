(uiop:define-package #:app/pages/admin/profiles
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:update
                #:render
                #:defwidget)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/vars
                #:*url-prefix*)
  (:import-from #:passport/client
                #:user-id
                #:make-passport)
  (:import-from #:openrpc-client/error
                #:rpc-error
                #:rpc-error-message)
  (:import-from #:app/forms
                #:with-html-form)
  (:import-from #:reblocks-ui/form
                #:render-form-and-button)
  (:import-from #:app/utils
                #:get-user-token)
  (:export #:make-admin-profiles-page))
(in-package #:app/pages/admin/profiles)


(defwidget admin-profiles-page ()
  ())


(defun make-admin-profiles-page ()
  (make-instance 'admin-profiles-page))


(defun render-profile (client widget user &aux (user-id (user-id user)))
  (flet ((block-user (&rest rest)
           (declare (ignore rest))
           (passport/client::block-user client user-id)
           (update widget))
         (admit-user (&rest rest)
           (declare (ignore rest))
           (passport/client::admit-user client user-id)
           (update widget))
         (make-user-admin (&rest rest)
           (declare (ignore rest))
           (passport/client::make-user-admin client user-id)
           (update widget)))
    (with-html
      (:tr
       (:td 
        (:img :class "small-avatar"
              :src (passport/client:user-avatar-url user)))
       (:td (passport/client:user-email user))
       (:td (passport/client:user-fio user))
       (:td :class "user-actions"
            (render-form-and-button :block
                                    #'block-user
                                    :value "Заблокировать"
                                    :button-class "button tiny alert")
            (render-form-and-button :admit
                                    #'admit-user
                                    :value "Пустить"
                                    :button-class "button tiny success")
            (render-form-and-button :make-admin
                                    #'make-user-admin
                                    :value "Сделать Админом"
                                    :button-class "button tiny secondary"))))))


(defmethod render ((widget admin-profiles-page))
  (with-html
    (let* ((client (passport/client::connect (make-passport)
                                             (get-user-token)))
           (profiles (passport/client::profiles-on-moderation client)))
      (cond
        (profiles
         (with-html
           (:table :class "profiles"
                   (:thead
                    (:tr (:th "Avatar")
                         (:th "Email")
                         (:th "ФИО")
                         (:th "Действия")))
                   (:tbody
                    (loop for user in profiles
                          do (render-profile client widget user))))))
        (t
         (:p "Пока нет новых пользователей для модерации."))))))


(defmethod get-dependencies ((widget admin-profiles-page))
  (list
   (reblocks-lass:make-dependency
     '(.admin-profiles-page
       :width 80%
       :margin-left auto
       :margin-right auto
       (.small-avatar
        :widget 25px
        :height 25px)
       (.user-actions
        :display flex
        :gap 0.5em)))))

