(uiop:define-package #:rates/rate/api
  (:use #:cl
        #:common/utils)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:rates/api
                #:rates-api)
  (:import-from #:rates/rate/model
                #:rate-value
                #:rate)
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
(in-package #:rates/rate/api)


(define-rpc-method (rates-api get-currency-rate) (currency)
  (:summary "Возвращает биржевой курс указанной валюты по отношению к рублю.")
  (:param currency string "Идентификатор валюты: USD, EUR, и тд.")
  (:result double-float)

  (with-connection (:database-name "rates")
    (let ((rate (first
                 (select-dao 'rate
                   (where 
                    (:= :currency currency))
                   (order-by (:desc :created-at))
                   (limit 1)))))
      (if rate
          (rate-value rate)
          0.0))))


(defclass rate-info ()
  ((timestamp :initarg :timestamp
              :initform 0
              :reader timestamp)
   (currency :initarg :currency
             :reader currency)
   (rate :initarg :rate
         :reader rate)))


(defmethod print-object ((obj rate-info) stream)
  (print-unreadable-object (obj stream)
    (format stream "[~A] ~A ~A"
            (local-time:unix-to-timestamp (timestamp obj))
            (currency obj)
            (rate obj))))


(define-rpc-method (rates-api get-latest-rates) ()
  (:summary "Возвращает биржевые курсы все известных валют.")
  (:result (list-of rate-info))

  (with-connection (:database-name "rates")
    (let ((rows (mito:retrieve-by-sql "
SELECT distinct currency,
       last_value(value) OVER (
           PARTITION BY currency
               ORDER BY created_at
          RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
       ) AS latest_value,
       extract(epoch from last_value(created_at) OVER (
           PARTITION BY currency
               ORDER BY created_at
          RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
       ))::integer AS timestamp
FROM rate
ORDER BY currency
")))
      (loop for row in rows
            collect (make-instance 'rate-info
                                   :timestamp (getf row :timestamp)
                                   :currency (getf row :currency)
                                   :rate (coerce (getf row :latest-value)
                                                 'double-float))))))


(define-rpc-method (rates-api get-history) (currency period candle-scale)
  (:summary "Возвращает историю изменения курса для указанной валюты.")
  (:description "Порядок сортировки - от более старых данных к более свежим.")
  (:param currency string "Код валюты.")
  (:param period integer "Количество секунд в прошлое, за которое нужно отдать данные.")
  (:param candle-scale integer "Количество секунд в одной свече.")
  (:result (list-of rate-info))

  (with-connection (:database-name "rates")
    (let ((rows (mito:retrieve-by-sql "
WITH all_values AS (
  SELECT value,
         extract(epoch from created_at)::integer AS timestamp
  FROM rate
  WHERE currency = ? AND created_at > to_timestamp(?)
), with_candles AS (
  SELECT timestamp / ? as candle, timestamp, value
    FROM all_values
), grouped AS (
  SELECT candle, min(timestamp) as timestamp, min(value) as value
    FROM with_candles
  GROUP BY candle
)
SELECT timestamp, value
  FROM grouped
ORDER BY timestamp
"
                                      :binds (list currency
                                                   (- (timestamp-to-unix (now))
                                                      period)
                                                   candle-scale))))
      (loop for row in rows
            collect (make-instance 'rate-info
                                   :timestamp (getf row :timestamp)
                                   :currency currency
                                   :rate (coerce (getf row :value)
                                                 'double-float))))))
