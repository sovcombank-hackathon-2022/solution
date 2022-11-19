(uiop:define-package #:rates/server
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:common/server)
  (:import-from #:common/token
                #:get-jwt-secret)
  (:import-from #:clerk)
  (:import-from #:openrpc-server/api
                #:define-api)
  (:import-from #:rates/api
                #:rates-api)
  (:import-from #:rates/rate/api)
  (:import-from #:rates/adapters/base
                #:setup-crons)
  (:import-from #:rates/adapters/random)
  (:export
   #:start-me
   #:stop-me))
(in-package #:rates/server)


(defvar *default-port* 8003)


(defun start-me (&key (port *default-port*)
                      (interface "localhost"))
  (setup-crons)
  (clerk:start)
  (common/server::start rates-api port
                        :interface interface))


(defun stop-me (&key (port *default-port*))
  (clerk:stop)
  (common/server::stop port))
