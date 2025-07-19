(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_INVALID_PRICE (err u402))
(define-constant ERR_INVALID_LOCATION (err u403))
(define-constant ERR_INVALID_ITEM (err u404))
(define-constant ERR_ALREADY_VOTED (err u405))
(define-constant ERR_PRICE_NOT_FOUND (err u406))
(define-constant ERR_INSUFFICIENT_STAKE (err u407))
(define-constant ERR_COOLDOWN_ACTIVE (err u408))
(define-constant ERR_ALERT_NOT_FOUND (err u409))
(define-constant ERR_ALERT_ALREADY_EXISTS (err u410))
(define-constant ERR_INVALID_ALERT_TYPE (err u411))
(define-constant ERR_ALERT_LIMIT_REACHED (err u412))

(define-data-var total-price-entries uint u0)
(define-data-var min-stake-amount uint u100)
(define-data-var price-cooldown uint u144)
(define-data-var contract-paused bool false)
(define-data-var total-alerts uint u0)
(define-data-var max-alerts-per-user uint u20)

(define-map price-data
    {item: (string-ascii 64), location: (string-ascii 64)}
    {
        price: uint,
        reporter: principal,
        timestamp: uint,
        votes-for: uint,
        votes-against: uint,
        stake-amount: uint,
        verified: bool
    }
)

(define-map reporter-stakes
    principal
    uint
)

(define-map reporter-cooldowns
    {reporter: principal, item: (string-ascii 64), location: (string-ascii 64)}
    uint
)

(define-map price-history
    {item: (string-ascii 64), location: (string-ascii 64), entry-id: uint}
    {
        price: uint,
        reporter: principal,
        timestamp: uint,
        verified: bool
    }
)

(define-map price-votes
    {voter: principal, item: (string-ascii 64), location: (string-ascii 64)}
    {vote: bool, stake: uint}
)

(define-map item-locations
    (string-ascii 64)
    (list 20 (string-ascii 64))
)

(define-map location-items
    (string-ascii 64)
    (list 50 (string-ascii 64))
)

(define-map reporter-reputation
    principal
    {total-reports: uint, verified-reports: uint, reputation-score: uint}
)

(define-map price-alerts
    {alert-id: uint}
    {
        user: principal,
        item: (string-ascii 64),
        location: (string-ascii 64),
        target-price: uint,
        alert-type: (string-ascii 10),
        is-active: bool,
        created-at: uint,
        triggered-at: (optional uint)
    }
)

(define-map user-alerts
    principal
    (list 20 uint)
)

(define-map alert-triggers
    {item: (string-ascii 64), location: (string-ascii 64)}
    (list 50 uint)
)

(define-public (submit-price (item (string-ascii 64)) (location (string-ascii 64)) (price uint))
    (let (
        (current-block stacks-block-height)
        (reporter tx-sender)
        (stake-amount (default-to u0 (map-get? reporter-stakes reporter)))
        (cooldown-key {reporter: reporter, item: item, location: location})
        (last-submission (default-to u0 (map-get? reporter-cooldowns cooldown-key)))
    )
    (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (>= stake-amount (var-get min-stake-amount)) ERR_INSUFFICIENT_STAKE)
    (asserts! (> price u0) ERR_INVALID_PRICE)
    (asserts! (> (len item) u0) ERR_INVALID_ITEM)
    (asserts! (> (len location) u0) ERR_INVALID_LOCATION)
    (asserts! (>= current-block (+ last-submission (var-get price-cooldown))) ERR_COOLDOWN_ACTIVE)
    
    (let (
        (entry-id (var-get total-price-entries))
        (price-key {item: item, location: location})
    )
    (map-set price-data price-key {
        price: price,
        reporter: reporter,
        timestamp: current-block,
        votes-for: u0,
        votes-against: u0,
        stake-amount: stake-amount,
        verified: false
    })
    
    (map-set price-history
        {item: item, location: location, entry-id: entry-id}
        {price: price, reporter: reporter, timestamp: current-block, verified: false}
    )
    
    (map-set reporter-cooldowns cooldown-key current-block)
    (var-set total-price-entries (+ entry-id u1))
    (update-item-locations item location)
    (update-location-items location item)
    (update-reporter-stats reporter)
    (trigger-price-alerts item location price)
    (ok entry-id)
    ))
)

(define-public (vote-on-price (item (string-ascii 64)) (location (string-ascii 64)) (vote-for bool) (stake uint))
    (let (
        (voter tx-sender)
        (vote-key {voter: voter, item: item, location: location})
        (price-key {item: item, location: location})
        (voter-stake (default-to u0 (map-get? reporter-stakes voter)))
    )
    (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? price-votes vote-key)) ERR_ALREADY_VOTED)
    (asserts! (>= voter-stake stake) ERR_INSUFFICIENT_STAKE)
    (asserts! (> stake u0) ERR_INVALID_PRICE)
    
    (match (map-get? price-data price-key)
        price-info
        (let (
            (new-votes-for (if vote-for (+ (get votes-for price-info) stake) (get votes-for price-info)))
            (new-votes-against (if vote-for (get votes-against price-info) (+ (get votes-against price-info) stake)))
        )
        (map-set price-data price-key (merge price-info {
            votes-for: new-votes-for,
            votes-against: new-votes-against
        }))
        
        (map-set price-votes vote-key {vote: vote-for, stake: stake})
        (ok true)
        )
        ERR_PRICE_NOT_FOUND
    )
    )
)

(define-public (verify-price (item (string-ascii 64)) (location (string-ascii 64)))
    (let (
        (price-key {item: item, location: location})
    )
    (match (map-get? price-data price-key)
        price-info
        (let (
            (votes-for (get votes-for price-info))
            (votes-against (get votes-against price-info))
            (total-votes (+ votes-for votes-against))
        )
        (if (and (> total-votes u0) (> votes-for votes-against))
            (begin
                (map-set price-data price-key (merge price-info {verified: true}))
                (update-reporter-verified (get reporter price-info))
                (ok true)
            )
            (ok false)
        )
        )
        ERR_PRICE_NOT_FOUND
    )
    )
)

(define-public (stake-tokens (amount uint))
    (let (
        (reporter tx-sender)
        (current-stake (default-to u0 (map-get? reporter-stakes reporter)))
    )
    (asserts! (> amount u0) ERR_INVALID_PRICE)
    (map-set reporter-stakes reporter (+ current-stake amount))
    (ok (+ current-stake amount))
    )
)

(define-public (withdraw-stake (amount uint))
    (let (
        (reporter tx-sender)
        (current-stake (default-to u0 (map-get? reporter-stakes reporter)))
    )
    (asserts! (>= current-stake amount) ERR_INSUFFICIENT_STAKE)
    (map-set reporter-stakes reporter (- current-stake amount))
    (ok (- current-stake amount))
    )
)

(define-public (pause-contract)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-paused true)
        (ok true)
    )
)

(define-public (unpause-contract)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-paused false)
        (ok true)
    )
)

(define-public (update-min-stake (new-amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set min-stake-amount new-amount)
        (ok new-amount)
    )
)

(define-public (update-cooldown (new-cooldown uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set price-cooldown new-cooldown)
        (ok new-cooldown)
    )
)

(define-public (create-price-alert (item (string-ascii 64)) (location (string-ascii 64)) (target-price uint) (alert-type (string-ascii 10)))
    (let (
        (user tx-sender)
        (alert-id (var-get total-alerts))
        (current-alerts (default-to (list) (map-get? user-alerts user)))
        (triggers (default-to (list) (map-get? alert-triggers {item: item, location: location})))
    )
    (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (> target-price u0) ERR_INVALID_PRICE)
    (asserts! (> (len item) u0) ERR_INVALID_ITEM)
    (asserts! (> (len location) u0) ERR_INVALID_LOCATION)
    (asserts! (or (is-eq alert-type "above") (is-eq alert-type "below") (is-eq alert-type "equal")) ERR_INVALID_ALERT_TYPE)
    (asserts! (< (len current-alerts) (var-get max-alerts-per-user)) ERR_ALERT_LIMIT_REACHED)
    
    (map-set price-alerts {alert-id: alert-id} {
        user: user,
        item: item,
        location: location,
        target-price: target-price,
        alert-type: alert-type,
        is-active: true,
        created-at: stacks-block-height,
        triggered-at: none
    })
    
    (map-set user-alerts user (unwrap! (as-max-len? (append current-alerts alert-id) u20) ERR_ALERT_LIMIT_REACHED))
    (map-set alert-triggers {item: item, location: location} (unwrap! (as-max-len? (append triggers alert-id) u50) ERR_ALERT_LIMIT_REACHED))
    (var-set total-alerts (+ alert-id u1))
    (ok alert-id)
    )
)

(define-public (cancel-price-alert (alert-id uint))
    (let (
        (user tx-sender)
    )
    (match (map-get? price-alerts {alert-id: alert-id})
        alert-info
        (begin
            (asserts! (is-eq (get user alert-info) user) ERR_UNAUTHORIZED)
            (asserts! (get is-active alert-info) ERR_ALERT_NOT_FOUND)
            (map-set price-alerts {alert-id: alert-id} (merge alert-info {is-active: false}))
            (remove-alert-from-user user alert-id)
            (ok true)
        )
        ERR_ALERT_NOT_FOUND
    )
    )
)

(define-public (reactivate-price-alert (alert-id uint))
    (let (
        (user tx-sender)
    )
    (match (map-get? price-alerts {alert-id: alert-id})
        alert-info
        (begin
            (asserts! (is-eq (get user alert-info) user) ERR_UNAUTHORIZED)
            (asserts! (not (get is-active alert-info)) ERR_ALERT_ALREADY_EXISTS)
            (map-set price-alerts {alert-id: alert-id} (merge alert-info {is-active: true, triggered-at: none}))
            (add-alert-to-user user alert-id)
            (ok true)
        )
        ERR_ALERT_NOT_FOUND
    )
    )
)

(define-public (update-alert-limit (new-limit uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set max-alerts-per-user new-limit)
        (ok new-limit)
    )
)

(define-read-only (get-price (item (string-ascii 64)) (location (string-ascii 64)))
    (map-get? price-data {item: item, location: location})
)

(define-read-only (get-verified-price (item (string-ascii 64)) (location (string-ascii 64)))
    (match (map-get? price-data {item: item, location: location})
        price-info
        (if (get verified price-info)
            (some price-info)
            none
        )
        none
    )
)

(define-read-only (get-price-history (item (string-ascii 64)) (location (string-ascii 64)) (entry-id uint))
    (map-get? price-history {item: item, location: location, entry-id: entry-id})
)

(define-read-only (get-reporter-stake (reporter principal))
    (default-to u0 (map-get? reporter-stakes reporter))
)

(define-read-only (get-reporter-reputation (reporter principal))
    (default-to {total-reports: u0, verified-reports: u0, reputation-score: u0} 
        (map-get? reporter-reputation reporter))
)

(define-read-only (get-item-locations (item (string-ascii 64)))
    (default-to (list) (map-get? item-locations item))
)

(define-read-only (get-location-items (location (string-ascii 64)))
    (default-to (list) (map-get? location-items location))
)

(define-read-only (get-contract-stats)
    {
        total-entries: (var-get total-price-entries),
        min-stake: (var-get min-stake-amount),
        cooldown-blocks: (var-get price-cooldown),
        paused: (var-get contract-paused),
        total-alerts: (var-get total-alerts),
        max-alerts-per-user: (var-get max-alerts-per-user)
    }
)

(define-read-only (get-price-alert (alert-id uint))
    (map-get? price-alerts {alert-id: alert-id})
)

(define-read-only (get-user-alerts (user principal))
    (default-to (list) (map-get? user-alerts user))
)

(define-read-only (get-active-alerts-for-item (item (string-ascii 64)) (location (string-ascii 64)))
    (let (
        (trigger-alerts (default-to (list) (map-get? alert-triggers {item: item, location: location})))
    )
    (filter-active-alerts trigger-alerts)
    )
)

(define-read-only (get-alert-statistics (user principal))
    (let (
        (user-alert-ids (default-to (list) (map-get? user-alerts user)))
        (active-count (len (filter-user-active-alerts user-alert-ids)))
        (total-count (len user-alert-ids))
    )
    {
        total-alerts: total-count,
        active-alerts: active-count,
        triggered-alerts: (- total-count active-count)
    }
    )
)

(define-read-only (can-submit-price (reporter principal) (item (string-ascii 64)) (location (string-ascii 64)))
    (let (
        (current-block stacks-block-height)
        (cooldown-key {reporter: reporter, item: item, location: location})
        (last-submission (default-to u0 (map-get? reporter-cooldowns cooldown-key)))
        (stake-amount (default-to u0 (map-get? reporter-stakes reporter)))
    )
    {
        can-submit: (and 
            (not (var-get contract-paused))
            (>= stake-amount (var-get min-stake-amount))
            (>= current-block (+ last-submission (var-get price-cooldown)))
        ),
        blocks-remaining: (if (>= current-block (+ last-submission (var-get price-cooldown)))
            u0
            (- (+ last-submission (var-get price-cooldown)) current-block)
        )
    }
    )
)

(define-private (update-item-locations (item (string-ascii 64)) (location (string-ascii 64)))
    (let (
        (current-locations (default-to (list) (map-get? item-locations item)))
    )
    (if (is-none (index-of current-locations location))
        (map-set item-locations item (unwrap! (as-max-len? (append current-locations location) u20) false))
        true
    )
    )
)

(define-private (update-location-items (location (string-ascii 64)) (item (string-ascii 64)))
    (let (
        (current-items (default-to (list) (map-get? location-items location)))
    )
    (if (is-none (index-of current-items item))
        (map-set location-items location (unwrap! (as-max-len? (append current-items item) u50) false))
        true
    )
    )
)

(define-private (update-reporter-stats (reporter principal))
    (let (
        (current-stats (default-to {total-reports: u0, verified-reports: u0, reputation-score: u0} 
            (map-get? reporter-reputation reporter)))
        (new-total (+ (get total-reports current-stats) u1))
    )
    (map-set reporter-reputation reporter {
        total-reports: new-total,
        verified-reports: (get verified-reports current-stats),
        reputation-score: (calculate-reputation-score new-total (get verified-reports current-stats))
    })
    )
)

(define-private (update-reporter-verified (reporter principal))
    (let (
        (current-stats (default-to {total-reports: u0, verified-reports: u0, reputation-score: u0} 
            (map-get? reporter-reputation reporter)))
        (new-verified (+ (get verified-reports current-stats) u1))
    )
    (map-set reporter-reputation reporter {
        total-reports: (get total-reports current-stats),
        verified-reports: new-verified,
        reputation-score: (calculate-reputation-score (get total-reports current-stats) new-verified)
    })
    )
)

(define-private (calculate-reputation-score (total-reports uint) (verified-reports uint))
    (if (> total-reports u0)
        (/ (* verified-reports u100) total-reports)
        u0
    )
)

(define-private (trigger-price-alerts (item (string-ascii 64)) (location (string-ascii 64)) (new-price uint))
    (let (
        (trigger-alerts (default-to (list) (map-get? alert-triggers {item: item, location: location})))
    )
    (fold check-and-trigger-alert trigger-alerts new-price)
    )
)

(define-private (check-and-trigger-alert (alert-id uint) (price uint))
    (match (map-get? price-alerts {alert-id: alert-id})
        alert-info
        (if (and (get is-active alert-info) (is-none (get triggered-at alert-info)))
            (if (should-trigger-alert (get alert-type alert-info) (get target-price alert-info) price)
                (begin
                    (map-set price-alerts {alert-id: alert-id} 
                        (merge alert-info {triggered-at: (some stacks-block-height)}))
                    price
                )
                price
            )
            price
        )
        price
    )
)

(define-private (should-trigger-alert (alert-type (string-ascii 10)) (target-price uint) (current-price uint))
    (if (is-eq alert-type "above")
        (>= current-price target-price)
        (if (is-eq alert-type "below")
            (<= current-price target-price)
            (is-eq current-price target-price)
        )
    )
)

(define-private (filter-active-alerts (alert-ids (list 50 uint)))
    (filter is-alert-active alert-ids)
)

(define-private (is-alert-active (alert-id uint))
    (match (map-get? price-alerts {alert-id: alert-id})
        alert-info
        (get is-active alert-info)
        false
    )
)

(define-private (filter-user-active-alerts (alert-ids (list 20 uint)))
    (filter is-user-alert-active alert-ids)
)

(define-private (is-user-alert-active (alert-id uint))
    (match (map-get? price-alerts {alert-id: alert-id})
        alert-info
        (get is-active alert-info)
        false
    )
)

(define-private (remove-alert-from-user (user principal) (alert-id uint))
    true
)

(define-private (add-alert-to-user (user principal) (alert-id uint))
    (let (
        (current-alerts (default-to (list) (map-get? user-alerts user)))
    )
    (if (< (len current-alerts) (var-get max-alerts-per-user))
        (map-set user-alerts user (unwrap! (as-max-len? (append current-alerts alert-id) u20) false))
        false
    )
    )
)
