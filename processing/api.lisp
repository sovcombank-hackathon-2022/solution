(uiop:define-package #:processing/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-api))
(in-package #:processing/api)


(define-api (processing-api :title "Processing API"))
