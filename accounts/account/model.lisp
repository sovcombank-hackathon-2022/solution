(uiop:define-package #:accounts/account/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:alexandria
                #:lastcar)
  (:import-from #:serapeum
                #:dict))
(in-package #:accounts/account/model)


(defclass account ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (user-id :initarg :user-id
            :type integer
            :col-type :integer
            :reader user-id)
   (currency :initarg :currency
             :type string
             :col-type :text
             :reader account-currency
             :documentation "Одно из значений из *currencies*.")
   (amount :initarg :subject-id
           :type double-float
           :col-type :decimal
           :inflate (lambda (item)
                      (coerce item 'double-float))
           :reader account-amount
           :documentation "ID объекта, за который оставлен голос."))
  (:documentation "Запись о количестве денег на счету пользователя.")
  (:metaclass dao-table-class))


(defmethod print-object ((obj account) stream)
  (print-unreadable-object (obj stream)
    (format stream "~A ~A"
            (account-amount obj)
            (account-currency obj))))


(defmethod openrpc-server:type-to-schema ((type (eql 'double-float)))
  (dict "type" "number"))


(defmethod openrpc-server/interface:transform-result ((obj float))
  obj)

(defmethod openrpc-server/interface:transform-result ((obj float))
  obj)
