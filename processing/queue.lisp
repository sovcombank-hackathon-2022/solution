(uiop:define-package #:processing/queue
  (:use #:cl)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:processing/order/model
                #:order-user-token
                #:order-account-id
                #:order-buy-or-sell
                #:account-id
                #:order-lots
                #:order-execution-price
                #:order-currency
                #:order-status
                #:order-type
                #:order)
  (:import-from #:rates/client
                #:make-rates)
  (:import-from #:app/utils
                #:get-user-token)
  (:import-from #:accounts/client
                #:add-operation
                #:get-current-user-accounts
                #:make-accounts)
  (:import-from #:common/cron
                #:job))
(in-package #:processing/queue)


(defun get-roubles-account (client)
  (loop for account in (get-current-user-accounts client)
        when (string-equal (accounts/client:account-currency account)
                           "RUB")
        do (return (accounts/client:account-id account))))


(defun process (order)
  (let ((app/utils:*token* (order-user-token order)))
    (uiop:while-collecting (collect-order)
      (cond
        ((string-equal (order-type order)
                       "market")
         ;; Тут у нас симуляция того, что заявка исполнилась, поэтому кидаем кубик и с вероятностью 30% совершаем покупку
         ;; по рыночной цене
         ;; TODO: пока отключил чтобы исполнялось быстрее
         (when (< (random 1.0) 1.0)
           (let* ((token (get-user-token))
                  (rates-client (when token
                                  (rates/client::connect
                                   (make-rates)
                                   token)))
                  (accounts-client (when token
                                     (accounts/client::connect
                                      (make-accounts)
                                      token)))
                  (current-price (rates/client:get-currency-rate rates-client
                                                                 (order-currency order)))
                  (lots (order-lots order))
                  ;; ID валютного счёта:
                  (account-id (order-account-id order))
                  (roubles-account-id (get-roubles-account accounts-client))
                  (total-price-in-roubles (* current-price
                                             lots))
                  (buy? (string-equal (order-buy-or-sell order)
                                      "buy")))
             (log:info "Order ~A was executed at ~A"
                       (mito:object-id order)
                       current-price)
            
             (setf (order-status order)
                   "executed")
             (setf (order-execution-price order)
                   current-price)

             ;; TODO: тут нужен будет двухфазный коммит, чтобы консистентно добавлять
             ;; операцию по счёту в микросервис accounts:
             (cond
               (buy?
                (add-operation accounts-client account-id (coerce lots 'double-float))
                (add-operation accounts-client roubles-account-id (coerce (- total-price-in-roubles)
                                                                          'double-float)))
               (t
                (add-operation accounts-client account-id (coerce  (- lots)
                                                                   'double-float))
                (add-operation accounts-client roubles-account-id (coerce total-price-in-roubles
                                                                          'double-float))))
            
             (mito:save-dao order)
             (collect-order order))))
        (t
         (log:warn "Orders of type ~A arent supported yet."
                   (order-type order)))))))


(defun process-pending-orders ()
  ;; TODO: для прода надо будет распилить процессинг на воркеры которые будут брать задачи из очереди
  ;; типа Кафки:
  (with-connection (:database-name "processing")
    (mapcar #'process (mito:select-dao 'order
                        (sxql:where (:= :status "active"))
                        (sxql:order-by :created-at)))))


(defun setup-cron ()
  (job "Process orders."
       every 5.second
       (process-pending-orders)))
