(uiop:define-package #:rates/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-api))
(in-package #:rates/api)


(define-api (rates-api :title "Rates API"))
