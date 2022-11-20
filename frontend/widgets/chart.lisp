(uiop:define-package #:app/widgets/chart
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:reblocks/widgets/dom
                #:dom-id)
  (:import-from #:reblocks-websocket
                #:websocket-widget)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:local-time
                #:now
                #:timestamp-to-unix)
  (:import-from #:app/utils
                #:get-user-token)
  (:import-from #:rates/client
                #:make-rates)
  (:import-from #:distributions
                #:draw
                #:r-normal)
  (:export
   #:make-chart-widget))
(in-package #:app/widgets/chart)


(defwidget chart (websocket-widget)
  ((currency :initarg :currency
             :initform "USD"
             :reader chart-currency)
   (candle-scale :initarg :candle-scale
                 :initform 60
                 :reader candle-scale
                 :documentation "Ширина свечи в графике (в секундах).")))


(defun make-chart-widget (currency &key (candle-scale 60))
  (make-instance 'chart
                 :currency currency
                 :candle-scale candle-scale))


(defun render-history (history)
  (flet ((rand (mean &optional (variance 0.1))
           (draw (r-normal mean variance))))
    (loop for item in history
          for close = (rates/client:rate-info-rate item)
          for open = (rand close)
          for low = (- (min open close)
                       (abs (rand 0.0)))
          for high = (+ (min open close)
                       (abs (rand 0.0)))
          collect (format nil "{time: ~A, open: ~A, high: ~A, low: ~A, close: ~A}"
                          (rates/client:rate-info-timestamp item)
                          (coerce open 'single-float)
                          (coerce high 'single-float)
                          (coerce low 'single-float)
                          (coerce close 'single-float)) into items
          finally (return
                    (format nil
                            "~{~A~^,~%~}"
                            items)))))


(defmethod reblocks/widget:render ((widget chart))
  (let* ((rates-api (rates/client::connect
                     (make-rates)
                     (get-user-token)))
         (period (* 24 3600))
         (history (rates/client:get-history  rates-api
                                             (chart-currency widget)
                                             period
                                             (candle-scale widget))))
    (with-html
      (:script (:raw (fmt "

var chart = LightweightCharts.createChart(document.getElementById('~A'), {
	width: 600,
        height: 450,
	crosshair: {
		mode: LightweightCharts.CrosshairMode.Normal,
	},
        timeScale: {
		timeVisible: true,
    secondsVisible: false,
	}
});

var candleSeries = chart.addCandlestickSeries();

var data = [
~A
];

candleSeries.setData(data);

function getRandomPrice() {
	return 10 + Math.round(Math.random() * 10000) / 100;
}


var lastClose = data[data.length - 1].close;
var lastIndex = data.length - 1;

var targetIndex = lastIndex + 105 + Math.round(Math.random() + 30);
var targetPrice = getRandomPrice();

var currentIndex = lastIndex + 1;
var currentBusinessDay = 1668882010; //{ day: 19, month: 11, year: 2022,  };
var ticksInCurrentBar = 0;
var currentBar = {
	open: null,
	high: null,
	low: null,
	close: null,
	time: currentBusinessDay,
};

function mergeTickToBar(time, price) {
    console.log('Updating curren bar');

        currentBar.time = time;
	if (currentBar.open === null) {
		currentBar.open = price;
		currentBar.high = price;
		currentBar.low = price;
		currentBar.close = price;
	} else {
		currentBar.close = price;
		currentBar.high = Math.max(currentBar.high, price);
		currentBar.low = Math.min(currentBar.low, price);
	}
	candleSeries.update(currentBar);
}




"
                          (dom-id widget)
                          (render-history history)))))))


(defun process-updates (widget)
  (with-log-unhandled ()
    (log:info "Updating" widget "via websocket.")
    (let* ((rates-api (rates/client::connect
                       (make-rates)
                       (get-user-token)))
           (rate (rates/client:get-currency-rate rates-api
                                                 (chart-currency widget)))
           (timestamp (* (ceiling
                          (/ (timestamp-to-unix (now))
                             (candle-scale widget)))
                         (candle-scale widget))))
      (reblocks-websocket:send-script
       `(ps:chain (merge-tick-to-bar
                   ,timestamp ,rate))))))


(defmethod initialize-instance :after ((widget chart) &rest rest)
  (declare (ignorable rest))

  (log:info "Initializing websocket for" widget)
  (reblocks-websocket:in-thread ("Update chart")
    (loop do (sleep 3)
             (process-updates widget))))


(defmethod get-dependencies ((widget chart))
  (let ((result (list*
                 (reblocks-lass:make-dependency
                   '(.chart))
                 (reblocks/dependencies:make-dependency
                   "https://unpkg.com/lightweight-charts/dist/lightweight-charts.standalone.production.js")
                 (call-next-method))))
    (values result)))
