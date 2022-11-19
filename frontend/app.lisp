(uiop:define-package #:app/app
  (:use #:cl)
  (:import-from #:reblocks-navigation-widget
                #:defroutes)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:app/pages/ping)
  (:import-from #:reblocks-lass)
  (:import-from #:app/pages/login
                #:make-login-page)
  (:import-from #:app/pages/landing
                #:make-landing-page)
  (:import-from #:app/vars
                #:*url-prefix*)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/pages/logout
                #:make-logout-page)
  (:import-from #:app/pages/profiles
                #:make-account-widget
                #:make-profiles-widget)
  (:import-from #:app/pages/jobs
                #:make-jobs-widget)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/widgets/header
                #:make-page-with-header)
  (:import-from #:app/pages/edit-profile
                #:make-edit-profile-widget)
  (:import-from #:app/pages/chat
                #:make-chat-page)
  (:import-from #:app/pages/signup
                #:make-signup-page)
  (:import-from #:app/pages/admin/profiles
                #:make-admin-profiles-page)
  (:import-from #:app/pages/accounts
                #:make-accounts-page)
  (:import-from #:app/pages/currency
                #:make-currency-page))
(in-package #:app/app)


(defapp app
  :prefix "/")


(defroutes routes
    ("/ping" (app/pages/ping::make-ping-page) )
  ("/signup/" (make-page-with-header
               (make-signup-page)))
  ("/login/" (make-page-with-header
              (make-login-page)) )
  ("/logout/" (make-page-with-header
               (make-logout-page)))
  ("/profiles/" (make-page-with-header
                 (make-profiles-widget)))
  ("/accounts/" (make-page-with-header
                 (make-accounts-page)))
  ("/currency/.*" (make-page-with-header
                   (make-currency-page)))
  ("/profiles/add/" (make-page-with-header
                     (make-edit-profile-widget)))
  ;; Админка
  ("/admin/profiles/" (make-page-with-header
                       (make-admin-profiles-page)))
  ("/" (make-page-with-header
        (make-landing-page))))


(defmethod reblocks/session:init ((app app))  
  (make-routes))


(defmethod get-dependencies ((app app))
  (append (list
           (reblocks-lass:make-dependency
             '()))
          (call-next-method)))
