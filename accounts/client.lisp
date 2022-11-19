(uiop:define-package #:accounts/client
  (:use #:cl)
  (:import-from #:openrpc-client)
  (:import-from #:common/rpc
                #:cached-url-as)
  (:export #:connect
           #:make-accounts))
(in-package #:accounts/client)


(openrpc-client:generate-client accounts (cached-url-as "http://localhost:8002/openrpc.json"
                                                        (asdf:system-relative-pathname :common "specs/rating.json")))

(defvar *client* (make-accounts))


(defun connect (client &optional token)
  (jsonrpc:client-connect client :mode :http :url "http://localhost:8002/"
                                 :headers (when token
                                            (list (cons :authorization
                                                        token))))
  client)
