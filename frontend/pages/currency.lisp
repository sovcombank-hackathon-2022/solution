(uiop:define-package #:app/pages/currency
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:update
                #:render
                #:defwidget)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/forms
                #:with-html-form)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:openrpc-client/error
                #:rpc-error
                #:rpc-error-message)
  (:import-from #:alexandria
                #:appendf)
  (:import-from #:rates/client
                #:make-rates)
  (:import-from #:app/utils
                #:get-user-token)
  (:import-from #:app/widgets/chart
                #:make-chart-widget)
  (:export
   #:make-currency-page))
(in-package #:app/pages/currency)


(defwidget currency ()
  ((ticker :initarg :ticker
           :initform ""
           :accessor currency-ticker)
   (chart :initarg :chart
          :reader chart-widget)))


(defun make-currency-page ()
  (let ((ticker "USD"))
    (make-instance 'currency
                   :ticker ticker
                   :chart  (make-chart-widget ticker))))


(defmethod render ((widget currency))
  (with-html
    (:h1 "История изменений")
    (render (chart-widget widget))))


(defmethod get-dependencies ((widget currency))
  (list
   (reblocks-lass:make-dependency
     '(.currency
       ))))
