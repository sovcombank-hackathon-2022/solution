(uiop:define-package #:app/pages/ping
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:export
   #:make-ping-page))
(in-package #:app/pages/ping)


(defwidget ping ()
  ())


(defun make-ping-page ()
  (make-instance 'ping))


(defmethod reblocks/widget:render ((ping ping))
  (with-html
    (:p "OK")))
