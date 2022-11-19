(uiop:define-package #:accounts/operation/api
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
                #:account-currency
                #:account-amount
                #:account)
  (:import-from #:accounts/operation/model
                #:operation)
  (:import-from #:accounts/api
                #:accounts-api)
  (:import-from #:common/currency
                #:*currencies*))
(in-package #:accounts/operation/api)


(define-rpc-method (accounts-api add-operation) (account-id amount)
  (:summary "Выполняет над счётом операцию пополнения или снятия. Отдаёт новый баланс аккаунта.")
  (:description "TODO: В будущем здесь надо навертеть всяких проверок, блокировок, да двухфазных коммитов.")
  (:param account-id integer)
  (:param amount double-float
          "В базе хранится как DECIMAL.")
  (:result double-float)

  (with-session (user-id)
    (with-connection (:database-name "accounts")
      (let ((account (find-dao 'account
                               :id account-id
                               :user-id user-id)))
        (unless account
          (return-error (fmt "Аккаунт с ID ~A не найден."
                             account-id)))

        (when (< (+ (account-amount account)
                    amount)
                 0)
          (return-error (fmt "Не хватает ~A ~A."
                             (- (+ amount
                                   (account-amount account)))
                             (account-currency account))))
        
        (create-dao 'operation
                    :account-id account-id
                    :amount amount)
        (mito:execute-sql "UPDATE account SET amount = amount + ?
                            WHERE id = ?"
                          (list amount
                                account-id))
        (coerce
         (getf (first (mito:retrieve-by-sql "SELECT amount FROM account WHERE id = ?"
                                            :binds (list account-id)))
               :amount)
         'double-float)))))
