(uiop:define-package #:rates/adapters/random
  (:use #:cl)
  (:import-from #:rates/adapters/base
                #:supported-currencies
                #:get-rate
                #:setup-cron
                #:update-rates)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:distributions
                #:r-normal
                #:draw)
  (:import-from #:common/cron
                #:job))
(in-package #:rates/adapters/random)


(defclass state ()
  ((min :initarg :min
        :reader min-value)
   (max :initarg :max
        :reader max-value)
   (trend :initarg :trend
          :accessor trend)
   (to-trend-change :initform 0
                    :accessor to-trend-change)
   (last-value :initform nil
               :initarg :last-value
               :accessor last-value))
  (:documentation "Состояние генератора котировки."))


(defmethod print-object ((state state) stream)
  (print-unreadable-object (state stream :type t)
    (format stream "LAST=~A, TREND=~A"
            (last-value state)
            (trend state))))


(defvar *states*
  (list (cons "USD" (make-instance 'state
                                   :last-value 60.5
                                   :min 59.0
                                   :max 62.0
                                   :trend 0.1))
        (cons "EUR" (make-instance 'state
                                   :last-value 62.75
                                   :min 61.0
                                   :max 63.5
                                   :trend -0.1))))


(defmethod supported-currencies ((adapter (eql :random)))
  (list "USD" "EUR"))


(defun generate-next-value (currency)
  (let* ((state (assoc-value *states*
                             currency
                             :test #'string-equal))
         (next-value (min
                      (max
                       (+ (last-value state)
                          (* (trend state)
                             (abs (draw (r-normal 0.0 1.5)))))
                       (min-value state))
                      (max-value state))))
    (setf (last-value state)
          next-value)
    (cond
      ;; Разворот тренда на границе
      ((or (< next-value (min-value state))
           (> (max-value state) next-value))
       (let ((new-trend (min
                         (max (- (* (trend state)
                                    2))
                              -0.5)
                         0.5)))
         (setf (trend state)
               new-trend))
       (setf (to-trend-change state)
             (+ (get-universal-time)
                (abs (draw (r-normal 60.0 (* 15 60.0)))))))
      ;; Разворот тренда по времени
      ((< (to-trend-change state)
          (get-universal-time))
       
       (setf (trend state)
             (let ((value (draw (r-normal 0.0 0.5))))
               (if (< value 0.0)
                   (- value 0.05)
                   (+ value 0.05))))
       (setf (to-trend-change state)
             (+ (get-universal-time)
                (abs (draw (r-normal 60.0 (* 15 60.0))))))))

    (values next-value
            state)))


(defmethod get-rate ((adapter (eql :random)) currency)
  (generate-next-value currency)
  ;; Старый алгоритм:
  ;; (cond
  ;;   ((string-equal currency "USD")
  ;;    (serapeum:random-in-range 60.0 61.0))
  ;;   (t
  ;;    (serapeum:random-in-range 62.0 63.0)))
  )


(defmethod setup-cron ((adapter (eql :random)))
  (job "Update random currency rates."
       every 15.second
       (update-rates adapter)))
