(uiop:define-package #:processing/order/api
  (:use #:cl
        #:common/utils)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:processing/api
                #:processing-api)
  (:import-from #:processing/order/model
                #:order)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:common/session
                #:with-session)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:mito
                #:retrieve-by-sql
                #:find-dao
                #:create-dao)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now))
(in-package #:processing/order/api)


(define-rpc-method (processing-api get-active-orders) ()
  (:summary "Возвращает все не исполнившиеся ордера текущего пользователя.")
  (:result (list-of order))

  (with-session (user-id)
    (with-connection (:database-name "processing")
      (select-dao 'order
        (where 
         (:= :user-id user-id))
        (order-by (:desc :created-at))))))



(define-rpc-method (processing-api create-order) (account-id currency buy-price order-type
                                                             &key limit-price)
  (:param account-id integer)
  (:param currency string)
  (:param order-type string)
  (:param buy-price double-float)
  (:param limit-price double-float)
  (:summary "Даёт новую заявку на покупку или продажу.")
  (:result order)

  (with-session (user-id)
    (with-connection (:database-name "processing")
      (create-dao 'order
                  :user-id user-id
                  :account-id account-id
                  :currency currency
                  :buy-price buy-price
                  :limit-price limit-price
                  :type order-type))))
