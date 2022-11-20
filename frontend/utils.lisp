(uiop:define-package #:app/utils
  (:use #:cl)
  (:import-from #:lack.request
                #:request-headers)
  (:import-from #:reblocks/request
                #:get-cookie)
  (:import-from #:reblocks/session
                #:get-value)
  (:import-from #:openrpc-server/vars
                #:*current-request*)
  (:export
   #:get-user-token
   #:*token*))
(in-package #:app/utils)


(defvar *token* nil)


(defun get-openrpc-token ()
  (when (boundp '*current-request*)
    (let* ((headers (request-headers *current-request*))
           (token (gethash "authorization" headers)))
      token)))


(defun get-user-token ()
  (or *token*
      (get-openrpc-token)
      (get-cookie "auth_token")
      (get-value :auth-token)))
