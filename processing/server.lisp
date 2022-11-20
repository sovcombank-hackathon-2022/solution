(uiop:define-package #:processing/server
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:common/server)
  (:import-from #:common/token
                #:get-jwt-secret)
  (:import-from #:clerk)
  (:import-from #:openrpc-server/api
                #:define-api)
  (:import-from #:processing/api
                #:processing-api)
  (:import-from #:processing/order/api)
  (:export #:start-me
           #:stop-me))
(in-package #:processing/server)


(defvar *default-port* 8004)


(defun start-me (&key (port *default-port*)
                      (interface "localhost"))
  (clerk:start)
  (common/server::start processing-api port
                        :interface interface))


(defun stop-me (&key (port *default-port*))
  (clerk:stop)
  (common/server::stop port))
