(uiop:define-package #:processing/client
  (:use #:cl)
  (:import-from #:openrpc-client)
  (:import-from #:common/rpc
                #:cached-url-as)
  (:export #:connect
           #:make-processing))
(in-package #:processing/client)


(openrpc-client:generate-client processing (cached-url-as "http://localhost:8004/openrpc.json"
                                                     (asdf:system-relative-pathname :common "specs/processing.json")))

(defvar *client* (make-processing))


(defun connect (client &optional token)
  (jsonrpc:client-connect client :mode :http :url "http://localhost:8004/"
                                 :headers (when token
                                            (list (cons :authorization
                                                        token))))
  client)
