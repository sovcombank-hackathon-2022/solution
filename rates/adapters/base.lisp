(uiop:define-package #:rates/adapters/base
  (:use #:cl)
  (:import-from #:rates/rate/model
                #:rate-source-title
                #:rate
                #:rate-source)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:common/db
                #:with-connection)
  (:export
   #:setup-crons))
(in-package #:rates/adapters/base)


(defgeneric supported-currencies (adapter)
  (:documentation "Возвращает курсы которые поддерживает адаптер."))


(defgeneric get-rate (adapter currency)
  (:documentation "Получает значение цены для указанной валюты в настоящий момент времени."))


(defcached (%get-source-cached :timeout 60) (adapter)
  (check-type adapter symbol)
  (mito:find-dao 'rate-source
                 :title (string-downcase adapter)))


(defgeneric get-source (adapter)
  (:documentation "Возвращает объект SOURCE по имени адаптера.")

  (:method ((adapter t))
    (%get-source-cached adapter)))


(defgeneric save-rate (adapter currency rate)
  (:documentation "Сохраняет значение в базу.")
  (:method ((adapter t) (currency t) (rate t))
    (mito:create-dao 'rate
                     :source (get-source adapter)
                     :currency currency
                     :value rate)))


(defgeneric update-rates (adapter)
  (:documentation "Обновляет курсы валют, выкачивая их с указанного адаптера.")
  (:method ((adapter t))
    (log:info "Updating rates for" adapter)
    
    (with-connection (:database-name "rates")
      (loop for currency in (supported-currencies adapter)
            for rate = (get-rate adapter currency)
            do (save-rate adapter currency rate)))))


(defgeneric setup-cron (adapter)
  (:documentation "Настраивает расписание для обновления котировок."))


(defun all-adapters ()
  (loop for item in (mito:retrieve-dao 'rate-source)
        collect (make-keyword (string-upcase (rate-source-title item)))))


(defun setup-crons ()
  "Настраивает расписание для всех адаптеров."
  (with-connection (:database-name "rates")
    (loop for adapter in (all-adapters)
          do (setup-cron adapter))))
