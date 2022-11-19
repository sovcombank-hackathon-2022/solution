(uiop:define-package #:app/pages/signup
  (:use #:cl)
  (:import-from #:reblocks/widget
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
                #:make-passport)
  (:import-from #:openrpc-client/error
                #:rpc-error
                #:rpc-error-message)
  (:import-from #:app/forms
                #:with-html-form))
(in-package #:app/pages/signup)


(defwidget signup-page ()
  ())


(defun make-signup-page ()
  (make-instance 'signup-page))


(defmethod render ((widget signup-page))
  (flet ((signup-user (&key email password fio &allow-other-keys)
           (log:info "Creating a new user in" email password fio)
           (handler-case
               (let* ((client (passport/client::connect (make-passport)))
                      (token (passport/client::signup client email password fio)))
                 (setf (reblocks/session:get-value :auth-token)
                       token)
                 (let ((url (fmt "~A/" *url-prefix*)))
                   (log:info "Redirecting user to" url)
                   (reblocks/response:redirect url)))
             (rpc-error (c)
               (reblocks-ui/form:form-error (rpc-error-message c))))))
    (with-html
      (with-html-form (:post #'signup-user)
        (:input :type "text"
                :name "fio"
                :placeholder "Имя, Фамилия и Отчество")
        (:input :type "text"
                :name "email"
                :placeholder "Ваш email")
        (:input :type "password"
                :name "password"
                :placeholder "Придумайте сложный пароль. Очень сложный!")
        (reblocks-ui/form:form-error-placeholder)

        (:div :class "submit-or-signup"
              (:input :type "submit"
                      :class "button"
                      :value "Создать")
              (:p "Или "
                  (:a :href "/login/"
                      "Залогиниться")))))))


(defmethod get-dependencies ((widget signup-page))
  (list
   (reblocks-lass:make-dependency
     '(.signup-page
       :width 50%
       :margin-left auto
       :margin-right auto

       (.submit-or-signup
        :display flex
        :gap 1em
        :align-items center)))))
