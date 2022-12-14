(uiop:define-package #:processing/order/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:alexandria
                #:lastcar))
(in-package #:processing/order/model)


(defparameter *order-types*
  (list "market"
        "stop-limit"
        "take-profit"))


(defparameter *order-statuses*
  (list "active"                        ;; ждет исполнения
        "executed"                      ;; исполнен полностью
        ))


(defclass order ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (user-id :initarg :user-id
            :type integer
            :col-type :bigint
            :reader order-user-id)
   (account-id :initarg :account-id
               :type integer
               :col-type :bigint
               :reader order-account-id)
   (buy-or-sell :initarg :buy-or-sell
                :type string
                :col-type :text
                :reader order-buy-or-sell)
   (type :initarg :type
         :type string
         :col-type :text
         :reader order-type)
   (status :initarg :status
           :type string
           :initform "active"
           :col-type :text
           :accessor order-status)
   (currency :initarg :currency
             :type string
             :col-type :text
             :reader order-currency
             :documentation "Одно из значений из *currencies*.")
   (lots :initarg :lots
         :initform nil
         :type integer
         :col-type :integer
         :reader order-lots
         :documentation "Сколько единиц валюты надо купить.")
   (limit-price :initarg :limit-price
                :initform nil
                :type (or null
                          double-float)
                :col-type (or :decimal
                              :null)
                :inflate (lambda (item)
                           (when item
                             (coerce item 'double-float)))
                :reader order-limit-price
                :documentation "Значение курса при котором должна сработать заявка.")
   (buy-price :initarg :buy-price
              :type (or null
                        double-float)
              :col-type (or :decimal
                            :null)
              :inflate (lambda (item)
                         (when item
                           (coerce item 'double-float)))
              :reader order-buy-price
              :documentation "Значение курса за который пользователь готов купить или продать.")
   (execution-price :initarg :execution-price
              :type (or null
                        double-float)
              :col-type (or :decimal
                            :null)
              :inflate (lambda (item)
                         (when item
                           (coerce item 'double-float)))
              :accessor order-execution-price
              :documentation "Значение курса по которому исполнилась заявка.")
   (user-token :initarg :user-token
         :type string
         :col-type :text
         :reader order-user-token))
  (:documentation "Запись об изменении курса.")
  (:metaclass dao-table-class))

