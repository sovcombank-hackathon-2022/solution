(uiop:define-package #:accounts/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-api)
  (:export #:accounts-api))
(in-package #:accounts/api)


(define-api (accounts-api :title "Accounts API"))
