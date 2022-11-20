(uiop:define-package #:app/pages/landing
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/session)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/utils
                #:get-user-token))
(in-package #:app/pages/landing)


(defwidget landing-page ()
  ())


(defun make-landing-page ()
  (make-instance 'landing-page))


(defmethod render ((widget landing-page))
  (let* ((token (get-user-token)))
    (cond
      (token
       (let* ((client (passport/client::connect (passport/client::make-passport) token))
              (profile (passport/client::my-profile client)))
         (with-html
           (:h1 ("Привет ~A!"
                 (passport/client::user-fio profile)))

           (:p "Тут должна быть какая-то начальная страница с дашбордиком, новостями валютного рынка и основными параметрами эффективности портфолио."))))
      (t
       (with-html
         (:h1 "Похоже, что нужно залогиниться."))))))
