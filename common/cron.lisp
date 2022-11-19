(uiop:define-package #:common/cron
  (:use #:cl)
  (:export
   #:job))
(in-package #:common/cron)


(defmacro job (name &rest body)
  `(unless (member ,name clerk:*jobs*
                   :test #'string-equal
                   :key #'clerk::name)
     (clerk:job ,name ,@body)))
