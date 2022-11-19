(uiop:define-package #:accounts/account/api
  (:use #:cl
        #:common/utils)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:common/session
                #:with-session)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:mito
                #:retrieve-by-sql
                #:find-dao
                #:create-dao)
  (:import-from #:accounts/account/model
                #:account)
  (:import-from #:accounts/api
                #:accounts-api)
  (:import-from #:common/currency
                #:*currencies*))
(in-package #:accounts/account/api)


(define-rpc-method (accounts-api get-current-user-accounts) ()
  (:summary "Показывает аккаунты текущего пользователя.")
  (:result (list-of account))

  (with-session (user-id)
    (with-connection (:database-name "accounts")
      (retrieve-dao 'account
                    :user-id user-id))))


(define-rpc-method (accounts-api add-account) (currency)
  (:summary "Показывает аккаунты текущего пользователя.")
  (:param currency string)
  (:result account)

  (unless (member currency *currencies* :test #'string-equal)
    (return-error (fmt "Пока поддерживаются только эти валюты: ~{~A~^, ~}"
                       *currencies*)))

  (with-session (user-id)
    (with-connection (:database-name "accounts")
      (let* ((currency (string-upcase currency))
             (exist (mito:find-dao 'account
                                   :user-id user-id
                                   :currency currency)))
        (when exist
          (return-error (fmt "Счёт в валюте ~A уже есть"
                             currency)))
        
        (create-dao 'account
                    :user-id user-id
                    :currency currency
                    :amount 0.0)))))
