(uiop:define-package #:accounts/operation/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:alexandria
                #:lastcar)
  (:import-from #:accounts/account/model
                #:account))
(in-package #:accounts/operation/model)


(defclass operation ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (account :initarg :account
            :type account
            :col-type account
            :reader operation-account)
   (amount :initarg :subject-id
           :type double-float
           :col-type :decimal
           :inflate (lambda (item)
                      (coerce item 'double-float))
           :reader subject-id
           :documentation "Если положительное значение, то приход - иначе - расход."))
  (:documentation "Запись об изменении количества денег на счету.")
  (:metaclass dao-table-class))

