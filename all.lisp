(uiop:define-package #:all/all
  (:use #:cl)
  (:import-from #:app/server)
  (:import-from #:passport/server)
  (:import-from #:rates/server)
  (:import-from #:accounts/server)
  (:import-from #:processing/server)
  (:export
   #:start-all))
(in-package #:all/all)


(defun start-all ()
  (when (probe-file "config.lisp")
    (load "config.lisp"))
  (app/server::start)
  (passport/server::start-me)
  (rates/server::start-me)
  (accounts/server::start-me)
  (processing/server::start-me)
  (values))
