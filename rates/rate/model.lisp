(uiop:define-package #:rates/rate/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:alexandria
                #:lastcar))
(in-package #:rates/rate/model)


(defclass rate-source ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (title :initarg :title
          :type string
          :col-type :text
          :reader rate-source-title))
  (:documentation "Источник из которого поступают данные о котировках.")
  (:metaclass dao-table-class))


(defclass rate ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (source :initarg :source
           :type string
           :col-type rate-source
           :reader rate-source)
   (currency :initarg :currency
             :type string
             :col-type :text
             :reader account-currency
             :documentation "Одно из значений из *currencies*.")
   (value :initarg :value
          :type double-float
          :col-type :decimal
          :inflate (lambda (item)
                     (coerce item 'double-float))
          :reader rate-value
          :documentation "Значение курса на указанный момент."))
  (:documentation "Запись об изменении курса.")
  (:metaclass dao-table-class))

