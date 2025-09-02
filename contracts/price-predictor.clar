;; title: Price Prediction Engine
;; version: 1.0.0
;; summary: ML-inspired price prediction system for forecasting future price movements
;; description: Uses historical data, volatility metrics, and trend analysis to predict future prices

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u500))
(define-constant err-not-found (err u501))
(define-constant err-unauthorized (err u502))
(define-constant err-invalid-amount (err u503))
(define-constant err-insufficient-data (err u504))
(define-constant err-prediction-not-found (err u505))
(define-constant err-model-not-trained (err u506))
(define-constant err-invalid-horizon (err u507))

;; prediction parameters
(define-constant min-data-points u5) ;; minimum historical data required
(define-constant max-prediction-horizon u20) ;; maximum blocks to predict ahead
(define-constant confidence-threshold u70) ;; minimum confidence for reliable predictions
(define-constant trend-weight u40) ;; weight of trend in prediction (40%)
(define-constant volatility-weight u30) ;; weight of volatility in prediction (30%)
(define-constant momentum-weight u30) ;; weight of momentum in prediction (30%)

;; data vars
(define-data-var next-prediction-id uint u1)
(define-data-var total-predictions uint u0)
(define-data-var successful-predictions uint u0)
(define-data-var prediction-accuracy uint u0)

;; data maps
(define-map price-predictions
  uint
  {
    item: (string-ascii 64),
    location: (string-ascii 64),
    predictor: principal,
    current-price: uint,
    predicted-price: uint,
    prediction-horizon: uint,
    confidence-score: uint,
    created-block: uint,
    target-block: uint,
    actual-price: (optional uint),
    accuracy-score: (optional uint),
    resolved: bool
  }
)

(define-map prediction-models
  {item: (string-ascii 64), location: (string-ascii 64)}
  {
    trend-factor: int,
    volatility-factor: uint,
    momentum-factor: int,
    accuracy-rate: uint,
    prediction-count: uint,
    successful-count: uint,
    last-updated: uint,
    model-trained: bool
  }
)

(define-map predictor-stats
  principal
  {
    total-predictions: uint,
    successful-predictions: uint,
    accuracy-rate: uint,
    confidence-average: uint,
    reputation-score: uint
  }
)

(define-map market-momentum
  {item: (string-ascii 64), location: (string-ascii 64)}
  {
    short-term-momentum: int,
    medium-term-momentum: int,
    momentum-strength: uint,
    last-calculated: uint
  }
)

;; public functions
(define-public (train-prediction-model (item (string-ascii 64)) (location (string-ascii 64)))
  (let ((current-block stacks-block-height)
        (volatility-data (contract-call? .Pricenet get-volatility-metrics item location))
        (model-key {item: item, location: location}))
    
    (match volatility-data
      vol-data (let ((price-count (get price-count vol-data))
                     (trend (get trend-indicator vol-data))
                     (volatility (get current-volatility vol-data)))
        
        (asserts! (>= price-count min-data-points) err-insufficient-data)
        
        (let ((trend-factor (calculate-trend-factor trend volatility))
              (volatility-factor (calculate-volatility-factor volatility))
              (momentum-data (calculate-momentum item location))
              (momentum-factor (get short-term-momentum momentum-data)))
          
          (map-set prediction-models model-key {
            trend-factor: trend-factor,
            volatility-factor: volatility-factor,
            momentum-factor: momentum-factor,
            accuracy-rate: u50, ;; start with 50% baseline
            prediction-count: u0,
            successful-count: u0,
            last-updated: current-block,
            model-trained: true
          })
          
          (map-set market-momentum model-key momentum-data)
          (ok true)
        ))
      err-insufficient-data)
  )
)

(define-public (make-price-prediction (item (string-ascii 64)) (location (string-ascii 64)) (prediction-horizon uint))
  (let ((prediction-id (var-get next-prediction-id))
        (current-block stacks-block-height)
        (predictor tx-sender)
        (model-key {item: item, location: location}))
    
    (asserts! (<= prediction-horizon max-prediction-horizon) err-invalid-horizon)
    
    (match (map-get? prediction-models model-key)
      model (let ((current-price-data (contract-call? .Pricenet get-verified-price item location)))
        
        (asserts! (get model-trained model) err-model-not-trained)
        
        (match current-price-data
          price-info (let ((current-price (get price price-info))
                          (predicted-price (calculate-predicted-price model current-price prediction-horizon))
                          (confidence (calculate-confidence-score model prediction-horizon)))
            
            (map-set price-predictions prediction-id {
              item: item,
              location: location,
              predictor: predictor,
              current-price: current-price,
              predicted-price: predicted-price,
              prediction-horizon: prediction-horizon,
              confidence-score: confidence,
              created-block: current-block,
              target-block: (+ current-block prediction-horizon),
              actual-price: none,
              accuracy-score: none,
              resolved: false
            })
            
            (update-predictor-stats predictor confidence)
            (var-set next-prediction-id (+ prediction-id u1))
            (var-set total-predictions (+ (var-get total-predictions) u1))
            (ok prediction-id)
          )
          err-not-found)
      )
      err-model-not-trained)
  )
)

(define-public (resolve-prediction (prediction-id uint))
  (let ((current-block stacks-block-height))
    
    (match (map-get? price-predictions prediction-id)
      prediction (let ((item (get item prediction))
                       (location (get location prediction))
                       (target-block (get target-block prediction))
                       (predicted-price (get predicted-price prediction)))
        
        (asserts! (not (get resolved prediction)) err-prediction-not-found)
        (asserts! (>= current-block target-block) err-invalid-horizon)
        
        (match (contract-call? .Pricenet get-verified-price item location)
          actual-price-data (let ((actual-price (get price actual-price-data))
                                 (accuracy (calculate-accuracy predicted-price actual-price))
                                 (is-successful (>= accuracy u60)))
            
            (map-set price-predictions prediction-id (merge prediction {
              actual-price: (some actual-price),
              accuracy-score: (some accuracy),
              resolved: true
            }))
            
            (update-model-performance item location is-successful)
            (if is-successful 
              (var-set successful-predictions (+ (var-get successful-predictions) u1))
              true)
            (update-prediction-accuracy)
            (ok accuracy)
          )
          err-not-found)
      )
      err-prediction-not-found)
  )
)

(define-public (update-prediction-parameters (min-data uint) (max-horizon uint) (confidence-thresh uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    ;; Note: In a real implementation, these would be stored in data-vars
    ;; For simplicity, we acknowledge the update request
    (ok true)
  )
)

;; read-only functions
(define-read-only (get-prediction (prediction-id uint))
  (map-get? price-predictions prediction-id)
)

(define-read-only (get-prediction-model (item (string-ascii 64)) (location (string-ascii 64)))
  (map-get? prediction-models {item: item, location: location})
)

(define-read-only (get-predictor-stats (predictor principal))
  (map-get? predictor-stats predictor)
)

(define-read-only (get-prediction-stats)
  {
    total-predictions: (var-get total-predictions),
    successful-predictions: (var-get successful-predictions),
    overall-accuracy: (var-get prediction-accuracy),
    next-prediction-id: (var-get next-prediction-id)
  }
)

(define-read-only (get-market-forecast (item (string-ascii 64)) (location (string-ascii 64)))
  (match (get-prediction-model item location)
    model (let ((momentum-data (default-to 
                                {short-term-momentum: 0, medium-term-momentum: 0, momentum-strength: u0, last-calculated: u0}
                                (map-get? market-momentum {item: item, location: location}))))
      (some {
        trend-outlook: (if (> (get trend-factor model) 0) "bullish" 
                          (if (< (get trend-factor model) 0) "bearish" "neutral")),
        volatility-level: (if (> (get volatility-factor model) u50) "high" "low"),
        momentum-strength: (get momentum-strength momentum-data),
        model-accuracy: (get accuracy-rate model),
        recommendation: (generate-trading-recommendation model momentum-data)
      }))
    none
  )
)

;; private functions
(define-private (calculate-trend-factor (trend int) (volatility uint))
  ;; Calculate trend strength considering volatility
  (if (is-eq trend 0)
    0
    (let ((base-factor (if (> trend 0) 100 -100))
          (volatility-adjustment (if (> volatility u30) 50 0)))
      (if (> trend 0)
        (- base-factor volatility-adjustment)
        (+ base-factor volatility-adjustment))
    )
  )
)

(define-private (calculate-volatility-factor (volatility uint))
  ;; Higher volatility increases uncertainty
  (if (> volatility u50)
    (+ u50 (/ volatility u2))
    (- u50 (/ volatility u2))
  )
)

(define-private (calculate-momentum (item (string-ascii 64)) (location (string-ascii 64)))
  ;; Simple momentum calculation based on recent price movements
  (let ((current-block stacks-block-height))
    {
      short-term-momentum: 0,  ;; simplified for demo
      medium-term-momentum: 0, ;; simplified for demo
      momentum-strength: u25,   ;; baseline momentum
      last-calculated: current-block
    }
  )
)

(define-private (calculate-predicted-price (model {trend-factor: int, volatility-factor: uint, momentum-factor: int, accuracy-rate: uint, prediction-count: uint, successful-count: uint, last-updated: uint, model-trained: bool}) (current-price uint) (horizon uint))
  ;; Combine trend, volatility, and momentum factors to predict price
  (let ((trend-factor-abs (if (>= (get trend-factor model) 0) 
                            (to-uint (get trend-factor model)) 
                            (to-uint (- (get trend-factor model)))))
        (trend-adjustment (/ (* current-price trend-factor-abs) u10000))
        (volatility-adjustment (/ (* current-price (get volatility-factor model)) u10000))
        (horizon-factor (/ (* u100 horizon) max-prediction-horizon)))
    
    (if (> (get trend-factor model) 0)
      (+ current-price (+ trend-adjustment (/ volatility-adjustment u2)))
      (if (< (get trend-factor model) 0)
        (if (> current-price (+ trend-adjustment (/ volatility-adjustment u2)))
          (- current-price (+ trend-adjustment (/ volatility-adjustment u2)))
          u1)
        current-price)
    )
  )
)

(define-private (calculate-confidence-score (model {trend-factor: int, volatility-factor: uint, momentum-factor: int, accuracy-rate: uint, prediction-count: uint, successful-count: uint, last-updated: uint, model-trained: bool}) (horizon uint))
  ;; Calculate confidence based on model accuracy and prediction difficulty
  (let ((base-confidence (get accuracy-rate model))
        (horizon-penalty (/ (* horizon u50) max-prediction-horizon))
        (volatility-penalty (/ (get volatility-factor model) u4)))
    
    (if (> (+ horizon-penalty volatility-penalty) base-confidence)
      u10
      (- base-confidence (+ horizon-penalty volatility-penalty))
    )
  )
)

(define-private (calculate-accuracy (predicted uint) (actual uint))
  ;; Calculate prediction accuracy percentage
  (let ((difference (if (> predicted actual) (- predicted actual) (- actual predicted)))
        (percentage-error (/ (* difference u100) actual)))
    
    (if (> percentage-error u100)
      u0
      (- u100 percentage-error)
    )
  )
)

(define-private (update-model-performance (item (string-ascii 64)) (location (string-ascii 64)) (successful bool))
  (let ((model-key {item: item, location: location}))
    
    (match (map-get? prediction-models model-key)
      model (let ((new-count (+ (get prediction-count model) u1))
                  (new-successful (if successful 
                                   (+ (get successful-count model) u1) 
                                   (get successful-count model)))
                  (new-accuracy (if (> new-count u0) 
                                 (/ (* new-successful u100) new-count) 
                                 u0)))
        
        (map-set prediction-models model-key (merge model {
          prediction-count: new-count,
          successful-count: new-successful,
          accuracy-rate: new-accuracy,
          last-updated: stacks-block-height
        }))
      )
      true
    )
  )
)

(define-private (update-predictor-stats (predictor principal) (confidence uint))
  (match (map-get? predictor-stats predictor)
    stats (map-set predictor-stats predictor (merge stats {
      total-predictions: (+ (get total-predictions stats) u1),
      confidence-average: (/ (+ (* (get confidence-average stats) (get total-predictions stats)) confidence)
                            (+ (get total-predictions stats) u1))
    }))
    (map-set predictor-stats predictor {
      total-predictions: u1,
      successful-predictions: u0,
      accuracy-rate: u0,
      confidence-average: confidence,
      reputation-score: u50
    })
  )
)

(define-private (update-prediction-accuracy)
  (let ((total (var-get total-predictions))
        (successful (var-get successful-predictions)))
    
    (if (> total u0)
      (var-set prediction-accuracy (/ (* successful u100) total))
      true
    )
  )
)

(define-private (generate-trading-recommendation (model {trend-factor: int, volatility-factor: uint, momentum-factor: int, accuracy-rate: uint, prediction-count: uint, successful-count: uint, last-updated: uint, model-trained: bool}) (momentum {short-term-momentum: int, medium-term-momentum: int, momentum-strength: uint, last-calculated: uint}))
  ;; Generate simple trading recommendation
  (if (and (> (get trend-factor model) 50) (> (get accuracy-rate model) u70))
    "buy"
    (if (and (< (get trend-factor model) -50) (> (get accuracy-rate model) u70))
      "sell"
      "hold"
    )
  )
)
