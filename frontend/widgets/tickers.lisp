(uiop:define-package #:app/widgets/tickers
  (:use #:cl)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  ;; (:import-from #:app/widgets/login
  ;;               #:get-username)
  (:import-from #:reblocks-ui/form
                #:render-form-and-button)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/utils
                #:get-user-token)
  (:import-from #:passport/client
                #:make-passport)
  (:import-from #:rates/client
                #:make-rates)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:reblocks-websocket
                #:websocket-widget)
  (:export
   #:make-page-with-header
   #:make-tickers-widget))
(in-package #:app/widgets/tickers)


(defwidget tickers (websocket-widget)
  ((rates :initform nil
          :accessor rates)))


(defun make-tickers-widget ()
  (make-instance 'tickers))


(defmethod reblocks/widget:render ((widget tickers))
  (with-html
    (:ul :class "tickers"
         (loop for rate in (rates widget)
               do (:li :class "rate"
                       (:span :class "currency"
                              (rates/client:rate-info-currency rate))
                       (:span :class "value"
                              (fmt "~,5G" (rates/client:rate-info-rate rate))))))
    ;; Для того, чтобы при обновлении котировки подсвечивались красным
    (:style (:raw "
        .tickers .rate {
            display: flex;
            gap: 0.3em;
        }
        .tickers .rate .value {
            animation: fadeInAnimation ease 3s;
            animation-iteration-count: 1;
            animation-fill-mode: forwards;
        }
        @keyframes fadeInAnimation {
            0% {
                color: red;
            }
            100% {
                color: gray;
            }
        }
"))))


(defun process-updates (widget)
  (log:info "Updating" widget "via websocket.")
  (ignore-errors
   (with-log-unhandled ()
     (let* ((rates-api (rates/client::connect
                        (make-rates)
                        (get-user-token)))
            (rates (rates/client:get-latest-rates rates-api)))
       (log:info "New Rates" rates)
       (setf (rates widget)
             rates)
       (reblocks/widget:update widget)))))


(defmethod initialize-instance :after ((widget tickers) &rest rest)
  (declare (ignorable rest))

  (log:info "Initializing websocket for" widget)
  (reblocks-websocket:in-thread ("Update tickers")
    (loop do (sleep 5)
             (process-updates widget))))


(defmethod get-dependencies ((widget tickers))
  (list*
   (reblocks-lass:make-dependency
     `(.tickers
       :margin-top -1rem
       :display flex                   
       :gap 1rem                       
       :list-style none                
       :font-size 0.7rem               
       :font-weight bold))
   (call-next-method)))
