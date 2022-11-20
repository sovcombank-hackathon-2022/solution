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
  (:import-from #:accounts/client
                #:make-accounts
                #:get-current-user-accounts)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:processing/order/model
                #:*order-types*)
  (:export
   #:make-currency-page))
(in-package #:app/pages/currency)


(defwidget order-form ()
  ())


(defwidget currency ()
  ((ticker :initarg :ticker
           :initform ""
           :accessor currency-ticker)
   (account-id :initarg :account-id
               :accessor account-id)
   (chart :initarg :chart
          :reader chart-widget)
   (order-form :initarg :order-form
               :reader order-form)))


(defun make-currency-page ()
  (cl-ppcre:register-groups-bind (ticker)
      ("^/currency/(.*)$" (get-path))
    (let* ((ticker (string-upcase ticker))
           (token (get-user-token))
           (client (when token
                     (accounts/client::connect
                      (make-accounts)
                      token)))
           (accounts (get-current-user-accounts client))
           (account-id (loop for account in accounts
                             when (string-equal (accounts/client:account-currency account)
                                                ticker)
                             do (return (accounts/client:account-id account)))))
      (unless account-id
        (error "Account for currency ~A not found."
               ticker))
      (make-instance 'currency
                     :ticker ticker
                     :chart (make-chart-widget ticker)
                     :order-form (make-instance 'order-form)))))


(defmethod render ((widget currency))
  (with-html
    (:h1 "История изменений")
    (render (chart-widget widget))
    (render (order-form widget))))


(defmethod render ((widget order-form))
  (flet ((add-order (&key &allow-other-keys)))
    (with-html
      (:h1 "Добавить заявку")
      (with-html-form (:post #'add-order)
        (:select :name "order-type"
          (loop for order-type in *order-types*
                do (:option :value order-type
                            order-type)))))))


(defmethod get-dependencies ((widget currency))
  (list
   (reblocks-lass:make-dependency
     '(.currency
       ))))
