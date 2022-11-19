(uiop:define-package #:rates/client
  (:use #:cl)
  (:import-from #:openrpc-client)
  (:import-from #:common/rpc
                #:cached-url-as)
  (:export #:connect
           #:make-rates))
(in-package #:rates/client)


(openrpc-client:generate-client rates (cached-url-as "http://localhost:8003/openrpc.json"
                                                     (asdf:system-relative-pathname :common "specs/rates.json")))

(defvar *client* (make-rates))


(defun connect (client &optional token)
  (jsonrpc:client-connect client :mode :http :url "http://localhost:8003/"
                                 :headers (when token
                                            (list (cons :authorization
                                                        token))))
  client)
