(uiop:define-package #:app/pages/currency
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
                #:parse-float
                #:fmt)
  (:import-from #:openrpc-client/error
                #:rpc-error
                #:rpc-error-message)
  (:import-from #:alexandria
                #:appendf)
  (:import-from #:rates/client
                #:make-rates)
  (:import-from #:app/utils
                #:get-user-token)
  (:import-from #:app/widgets/chart
                #:make-chart-widget)
  (:import-from #:accounts/client
                #:make-accounts
                #:get-current-user-accounts)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:processing/order/model
                #:*order-types*)
  (:import-from #:processing/client
                #:make-processing)
  (:import-from #:str
                #:non-empty-string-p)
  (:export
   #:make-currency-page))
(in-package #:app/pages/currency)


(defwidget order-form ()
  ((currency :initarg :currency
             :initform "USD"
             :reader currency)
   (account-id :initarg :account-id
               :accessor account-id)
   (widgets-to-update :initarg :widgets-to-update
                      :initform nil
                      :reader widgets-to-update)))


(defwidget order-widget ()
  ((order :initarg :raw
          :accessor raw-order)))


(defun make-order-widget (raw-order)
  (make-instance 'order-widget
                 :raw raw-order))


(defwidget active-orders ()
  ((account-id :initarg :account-id
               :accessor account-id)))


(defwidget currency ()
  ((ticker :initarg :ticker
           :initform ""
           :accessor currency-ticker)
   (account-id :initarg :account-id
               :accessor account-id)
   (chart :initarg :chart
          :reader chart-widget)
   (order-form :initarg :order-form
               :reader order-form)
   (active-orders :initarg :active-orders
                  :initform nil
                  :reader active-orders-widget)))


(defun make-currency-page ()
  (cl-ppcre:register-groups-bind (ticker)
      ("^/currency/(.*)$" (get-path))
    (let* ((ticker (string-upcase ticker))
           (token (get-user-token))
           (client (when token
                     (accounts/client::connect
                      (make-accounts)
                      token)))
           (accounts (get-current-user-accounts client))
           (account-id (loop for account in accounts
                             when (string-equal (accounts/client:account-currency account)
                                                ticker)
                             do (return (accounts/client:account-id account))))
           (chart-widget (make-chart-widget ticker))
           (active-orders-widget (make-instance 'active-orders
                                                :account-id account-id))
           (order-form (make-instance 'order-form
                                      :widgets-to-update (list active-orders-widget)
                                      :currency ticker
                                      :account-id account-id)))
      (unless account-id
        (error "Account for currency ~A not found."
               ticker))
      (make-instance 'currency
                     :ticker ticker
                     :chart chart-widget
                     :order-form order-form
                     :active-orders active-orders-widget))))


(defmethod render ((widget currency))
  (with-html
    (:div :class "column"
          (:div :class "row"
                (:div :class "column"
                      (:h1 (fmt "История изменений"
                                (currency widget)))
                      (render (chart-widget widget)))
                (render (order-form widget)))
          (when (active-orders-widget widget)
            (render (active-orders-widget widget))))))


(defmethod render ((widget active-orders))
  (with-html
    (let* ((token (get-user-token))
           (client (when token
                     (processing/client::connect
                      (make-processing)
                      token)))
           (active-orders (processing/client:get-active-orders client))
           (executed-orders (processing/client:get-executed-orders client))
           (order-widgets (mapcar #'make-order-widget
                                  active-orders))
           (executed-order-widgets (mapcar #'make-order-widget
                                           executed-orders)))
      (:div :class "column"
            (:h1 "Текущие заявки")
            (cond
              (order-widgets (mapc #'render order-widgets))
              (t
               (:p "Разместите заявку и она появится в этом разделе.")))
            
            (when executed-order-widgets
              (:h1 "Исполненные заявки")
              (mapc #'render executed-order-widgets))))))


(defmethod render ((widget order-widget))
  (with-html
    (let ((order (raw-order widget)))
      (:div :class (fmt "order ~A"
                        (processing/client:order-buy-or-sell order))
            (:span :class "buy-or-sell"
                   (if (string-equal (processing/client:order-buy-or-sell order)
                                     "buy")
                       "Купить"
                       "Продать"))
            (:span :class "lots"
                   (processing/client:order-lots order))
            (:span :class "currency"
                   (processing/client:order-currency order))
            (:span :class "order-type"
                   (if (string-equal (processing/client:order-type order)
                                     "market")
                       "По рыночной"
                       "С лимитом"))
            (cond
              ((string-equal (processing/client:order-status order)
                             "active")
               (:span :class "controls"
                      (reblocks-ui/form:render-form-and-button :Отменить
                                                               (lambda (&key &allow-other-keys)
                                                                 (reblocks/response:send-script "alert(\"Отмена пока не реализована.\")"))
                                                               :button-class "button secondary tiny")))
              (t
               (:span (fmt "Исполнено по: ~,4G"
                           (coerce (processing/client:order-execution-price order)
                                   'single-float)))))))))


(defmethod render ((widget order-form))
  (flet ((add-order (&key lots buy-price order-type limit-price
                          buy-or-sell
                     &allow-other-keys)
           (let* ((account-id (account-id widget))
                  (currency (currency widget))
                  (token (get-user-token))
                  (client (when token
                            (processing/client::connect
                             (make-processing)
                             token)))
                  (lots (when (non-empty-string-p lots)
                          (parse-integer lots)))
                  (buy-price (when (non-empty-string-p buy-price)
                               (parse-float buy-price)))
                  (limit-price (when (non-empty-string-p limit-price)
                                 (parse-float limit-price))))
             (log:info "Adding an order" account-id currency lots buy-price limit-price)

             (processing/client:create-order client
                                             account-id
                                             currency
                                             order-type
                                             lots
                                             :buy-or-sell buy-or-sell
                                             :buy-price buy-price
                                             :limit-price limit-price)
             (loop for widget in (list* widget
                                        (widgets-to-update widget))
                   do (reblocks/widget:update widget)))))
    (with-html
      (:h1 "Выставить заявку")
      (with-html-form (:post #'add-order)
        (:select :name "order-type"
          (loop for order-type in *order-types*
                do (:option :value order-type
                            (cond
                              ((string-equal order-type "market")
                               "По рыночной")
                              ((string-equal order-type "stop-limit")
                               "Stop Limit")
                              ((string-equal order-type "take-profit")
                               "Take Profit")
                              (t
                               order-type)))))
        
        (:select :name "buy-or-sell"
          (:option :value "buy"
                   :selected t
                   "Купить")
          (:option :value "sell"
                   "Продать"))
        
        (:label "Количество лотов:")
        
        (:input :type "number"
                :name "lots"
                :placeholder (fmt "Сколько ~A вы хотите купить?"
                                  (currency widget)))
        
        (:h5 "Для лимитных заявок:")
        (:label "Пороговая цена:")
        (:input :type "number"
                :name "limit-price"
                :placeholder "Если заявка stop-limit или take-profit.")
        (:label "Цена покупки:")
        (:input :type "number"
                :name "buy-price"
                :placeholder "За сколько вы готовы купить.")

        (:input :type "submit"
                :name "submit"
                :class "button success"
                :value "Разместить заявку")))))


(defmethod get-dependencies ((widget currency))
  (list
   (reblocks-lass:make-dependency
     '(.currency
       (.row
        :display flex
        :gap 2rem)))))


(defmethod get-dependencies ((widget order-widget))
  (list
   (reblocks-lass:make-dependency
     '(.order-widget
       (.order
        :margin-top 0.5em
        :margin-bottom 0.5em
        :padding 0.5em
        :border-radius 0.3em
        :border 1px solid lightblue
        :display flex
        :gap 1em
        :justify-content space-between
        :max-width 40%)
       ((:and .order .buy)
        (.buy-or-sell :color green))
       ((:and .order .sell)
        (.buy-or-sell :color red))))))
