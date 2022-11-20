(uiop:define-package #:app/widgets/header
  (:use #:cl)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  ;; (:import-from #:app/widgets/login
  ;;               #:get-username)
  (:import-from #:reblocks-ui/form
                #:render-form-and-button)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/utils
                #:get-user-token)
  (:import-from #:passport/client
                #:make-passport)
  (:import-from #:rates/client
                #:make-rates)
  (:import-from #:app/widgets/tickers
                #:make-tickers-widget)
  (:export
   #:make-page-with-header))
(in-package #:app/widgets/header)


(defwidget page-with-header ()
  ((content :initarg :content
            :reader content)
   (tickers :initform (make-tickers-widget)
            :reader tickers)))


(defun make-page-with-header (content)
  (make-instance 'page-with-header :content content))


(defmethod render ((widget page-with-header))
  (flet ((logout (&rest rest)
           (declare (ignore rest))
           (reblocks/session:reset)
           (reblocks/response:redirect "/logout/"))
         (login (&rest rest)
           (declare (ignore rest))
           (reblocks/response:redirect "/login/")))
    (let* ((api (passport/client::connect
                 (make-passport)
                 (get-user-token)))
           ;; (rates-api (rates/client::connect
           ;;             (make-rates)
           ;;             (get-user-token)))
           ;; (rates (rates/client:get-latest-rates rates-api))
           (profile (ignore-errors
                     (passport/client::my-profile api)))
           (avatar-url (when profile
                         (passport/client::user-avatar-url profile)))
           (admin? (when profile
                     (passport/client::user-admin profile))))
      (reblocks/html:with-html
        (:header
         (:div :class "navbar"
               (:div :class "navbar-main-logo"
                     "SovComTrade")
               (:div :class "navbar-right-block"
                     (:nav :class "menu main-menu"
                           (when (and profile
                                      (not admin?))
                             (:a :class "navbar-menu-point active"
                                 :href "/accounts/"
                                 "Счета")
                             ;; (:a :class "navbar-menu-point"
                             ;;     :href "/services/"
                             ;;     "История операций")
                             )
                           (when admin?
                             (:a :class "navbar-menu-point"
                                 :href "/admin/profiles/"
                                 "Модерация профилей")))
                     ;; Иконка профиля
                     (:nav :class "menu login-menu"
                           (cond
                             (avatar-url
                              (let ((avatar-classes (list* "navbar-user-icon"
                                                           (when admin?
                                                             (list "admin")))))
                                (:img :class (str:join " " avatar-classes)
                                      :src avatar-url))
                              (:a :class "logout-link"
                                  :href "/logout/"
                                  "Выйти"))
                             (t
                              (:a :class "login-link"
                                  :href "/login/"
                                  "Войти")))))))

        (when (tickers widget)
          (render (tickers widget)))

        (:div :class "page-content"
              (render (content widget)))))))



(defmethod get-dependencies ((widget page-with-header))
  (list
   (reblocks-lass:make-dependency
     `(.page-with-header
       :display flex
       :flex-direction column
       :align-items center

       ((:or .navbar-user-icon
             .login-link)
        :margin-left 3em)
       
       (.page-content
        :width 80%
        :margin-left auto
        :margin-right auto)
       
       (header
        :width 100%
        :border-bottom 1px solid "#c4c4c4" 
        :padding-bottom 1rem             
        :margin-bottom 2rem              
        :padding-left 1rem               
        :padding-top 1rem                
        :padding-right 1rem
        :background "#f3f4f7"
        
        (.navbar
         :display flex
         :width 100%
         (.navbar-right-block
          :display flex
          :width 100%)
         (.main-menu
          :flex-grow 10)
         (.navbar-main-logo
          :font-size 1.6em
          :font-family "Arial Black"
          :font-weight bold)
         (.navbar-user-icon
          :width 50px
          :height 50px)
         ((:and .navbar-user-icon
                .admin)
          :border 3px solid gold)))))))
