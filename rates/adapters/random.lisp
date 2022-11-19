(uiop:define-package #:rates/adapters/random
  (:use #:cl)
  (:import-from #:rates/adapters/base
                #:supported-currencies
                #:get-rate
                #:setup-cron
                #:update-rates))
(in-package #:rates/adapters/random)


(defmethod supported-currencies ((adapter (eql :random)))
  (list "USD" "EUR"))


(defmethod get-rate ((adapter (eql :random)) currency)
  (cond
    ((string-equal currency "USD")
     (serapeum:random-in-range 60.0 61.0))
    (t
     (serapeum:random-in-range 62.0 63.0))))


(defmethod setup-cron ((adapter (eql :random)))
  (common/cron:job "Update random currency rates."
                   every 15.second
                   (update-rates adapter)))
