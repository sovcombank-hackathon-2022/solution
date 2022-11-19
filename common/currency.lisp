(uiop:define-package #:common/currency
  (:use #:cl)
  (:export #:*currencies*))
(in-package #:common/currency)


(defparameter *currencies*
  '("RUB" "USD" "EUR" "GBP")
  "Пока захардкодим. Позже надо брать список из базы.")



