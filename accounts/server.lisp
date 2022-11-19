(uiop:define-package #:accounts/server
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:common/server)
  (:import-from #:common/token
                #:get-jwt-secret)
  (:import-from #:openrpc-server/api
                #:define-api)
  (:import-from #:accounts/api
                #:accounts-api)
  (:import-from #:accounts/account/api)
  (:import-from #:accounts/operation/api)
  (:export
   #:start-me
   #:stop-me))
(in-package #:accounts/server)


(defvar *default-port* 8002)


(defun start-me (&key (port *default-port*)
                   (interface "localhost"))
  (common/server::start accounts-api port
                        :interface interface))


(defun stop-me (&key (port *default-port*))
  (common/server::stop port))
