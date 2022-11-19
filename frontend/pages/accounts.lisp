(uiop:define-package #:app/pages/accounts
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:update
                #:render
                #:defwidget)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/forms
                #:with-html-form)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:openrpc-client/error
                #:rpc-error
                #:rpc-error-message)
  (:import-from #:alexandria
                #:appendf)
  (:import-from #:passport/user)
  (:import-from #:accounts/client
                #:add-operation
                #:make-accounts)
  (:import-from #:app/utils
                #:get-user-token)
  (:import-from #:common/currency
                #:*currencies*)
  (:import-from #:reblocks-ui/form
                #:form-error
                #:form-error-placeholder
                #:render-form-and-button)
  (:import-from #:reblocks-ui/popup
                #:hide-popup
                #:render-popup-content
                #:popup-widget)
  (:import-from #:parse-float
                #:parse-float)
  (:import-from #:jsonrpc
                #:jsonrpc-error-message
                #:jsonrpc-error)
  (:import-from #:common/session
                #:when-client)
  (:export
   #:make-accounts-page))
(in-package #:app/pages/accounts)


(defwidget account ()
  ((id :initarg :id
       :reader account-id)
   (currency :initarg :currency
             :reader account-currency)
   (amount :initarg :amount
           :accessor account-amount)))


(defwidget accounts ()
  ())


(defwidget withdraw-popup (popup-widget)
  ((parent :initarg :parent
           :reader parent)
   (account-id :initarg :account-id
               :reader account-id)))


(defwidget deposit-popup (popup-widget)
  ((parent :initarg :parent
           :reader parent)
   (account-id :initarg :account-id
               :reader account-id)))


(defun make-accounts-page ()
  (make-instance 'accounts))


(defun make-account-widget (obj)
  (make-instance 'account
                 :id (accounts/client:account-id obj)
                 :currency (accounts/client:account-currency obj)
                 :amount (accounts/client:account-amount obj)))


(defmethod render ((widget accounts))
  (let* ((token (get-user-token))
         (client (when token
                   (accounts/client::connect
                    (make-accounts)
                    token))))

    (when-client ()
      (with-html
        (:h3 "Ваши счета:")
        (let* ((accounts (accounts/client::get-current-user-accounts client))
               (available-currencies (set-difference *currencies*
                                                     (mapcar #'accounts/client::account-currency
                                                             accounts)
                                                     :test #'string-equal)))
          (cond
            (accounts
             (:div :class "accounts-header"
                   (:div :class "currency" "Валюта")
                   (:div :class "amount" "Количество")
                   (:div :class "operations" "Операции"))
             
             (loop for account in accounts
                   for widget = (make-account-widget account)
                   do (render widget)))
            (t
             (:p "У вас не открыто ни одного счёта.")))

          (:h3 "Добавить новый счёт:")
          (flet ((add-account (&key currency &allow-other-keys)
                   (log:info "Adding account with" currency)
                   (accounts/client:add-account client currency)
                   (update widget)))
            (with-html-form (:post #'add-account)
              (:select :name "currency"
                (loop for currency in available-currencies
                      do (:option :value currency
                                  currency)))
              (:input :type "submit"
                      :name "submit"
                      :class "button success"
                      :value "Добавить!"))))))))


(defmethod render ((widget account))
  (let ((client (accounts/client::connect
                 (make-accounts)
                 (get-user-token)))
        (deposit-popup (make-instance 'deposit-popup
                                      :account-id (account-id widget)
                                      :parent widget))
        (withdraw-popup (make-instance 'withdraw-popup
                                       :account-id (account-id widget)
                                       :parent widget)))
    (declare (ignore client))
    
    (labels ((show-withdraw-popup (&rest rest)
               (declare (ignore rest))
               (reblocks-ui/popup:show-popup withdraw-popup))
             (show-deposit-popup (&rest rest)
               (declare (ignore rest))
               (reblocks-ui/popup:show-popup deposit-popup)))
      (with-html
        (:div :class "currency"
              (account-currency widget))
        (:div :class "amount"
              (ceiling (account-amount widget)))
        (:div :class "controls"
              (render deposit-popup)
              (render withdraw-popup)

              (cond
                ((string-equal (account-currency widget)
                               "RUB")
                 (render-form-and-button :withdraw #'show-withdraw-popup
                                         :value "Снять"
                                         :button-class "button tiny secondary")
                 (render-form-and-button :withdraw #'show-deposit-popup
                                         :value "Пополнить"
                                         :button-class "button tiny success"))
                (t
                 (:a :href (fmt "/currency/~A"
                                (account-currency widget))
                     :class "button tiny"
                     "Купить"))))))))


(defmethod render-popup-content ((widget deposit-popup))
  (flet ((deposit-money (&key close-button amount &allow-other-keys)
           (cond
             (close-button
              (hide-popup widget))
             (t
              (let* ((client (accounts/client::connect
                              (make-accounts)
                              (get-user-token)))
                     (new-amount
                       (add-operation client
                                      (account-id widget)
                                      (parse-float amount
                                                   :type 'double-float))))
                (setf (account-amount (parent widget))
                      new-amount)
                (update (parent widget))
                (hide-popup widget))))))
    (with-html-form (:post #'deposit-money)
      (:h1 "Ведите сумму для пополнения:")
      (:h2 "Позже тут будет прикручена форма для ввода данных карточки или выбора карточных счетов Совкомбанка.")
      (:input :type "number"
              :name "amount"
              :min 1)
      (:input :type "submit"
              :class "button success"
              :value "Пополнить")
      (:input :type "submit"
              :class "button seconday"
              :name "close-button"
              :value "Отменить"))))


(defmethod render-popup-content ((widget withdraw-popup))
  (flet ((withdraw-money (&key amount close-button &allow-other-keys)
           (cond
             (close-button
              (hide-popup widget))
             (t
              (handler-case
                  (let* ((client (accounts/client::connect
                                  (make-accounts)
                                  (get-user-token)))
                         (new-amount
                           (add-operation client
                                          (account-id widget)
                                          (- (parse-float amount
                                                          :type 'double-float)))))
                    (setf (account-amount (parent widget))
                          new-amount)
                    (update (parent widget))
                    (hide-popup widget))
                (rpc-error (c)
                  (log:error "JSON-RPC error" c)
                  (form-error (rpc-error-message c)))
                (error (c)
                  (log:error "Unhandled error" c)
                  (form-error (serapeum:fmt "~A" c))))))))
    
    (with-html-form (:post #'withdraw-money)
      (:h1 "Ведите сумму для снятия:")
      (:h2 "Позже тут будет прикручена форма для заполнения банковских реквизитов, куда вывести деньги.")
      (:input :type "number"
              :name "amount"
              :min 1)
      (form-error-placeholder)
      
      (:input :type "submit"
              :class "button success"
              :value "Снять")
      (:input :type "submit"
              :class "button seconday"
              :name "close-button"
              :value "Отменить"))))


(defmethod render-popup-content ((widget withdraw-popup))
  (with-html
    (:p "Not implemented")))


(defmethod get-dependencies ((widget accounts))
  (list
   (reblocks-lass:make-dependency
     '(.accounts
       (.accounts-header
        :display flex
        :width 100%
        :justify-content space-between
        :gap 1em
        (.currency
         :min-width 5em)
        (.amount
         :flex-grow 10))
       
       (.controls
        :display flex
        :gap 1em)
       
       (.account
        :display flex
        :justify-content space-between
        :gap 1em
        (.amount
         :flex-grow 10)
        
        (.currency
         :min-width 5em
         :color blueviolet))
       
       ((:or .deposit-popup
         .withdraw-popup)
        (h1 :font-size 1.2rem)
        (h2 :font-size 0.7rem))))))
