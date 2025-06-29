(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_INVALID_PRICE (err u402))
(define-constant ERR_INVALID_LOCATION (err u403))
(define-constant ERR_INVALID_ITEM (err u404))
(define-constant ERR_ALREADY_VOTED (err u405))
(define-constant ERR_PRICE_NOT_FOUND (err u406))
(define-constant ERR_INSUFFICIENT_STAKE (err u407))
(define-constant ERR_COOLDOWN_ACTIVE (err u408))

(define-data-var total-price-entries uint u0)
(define-data-var min-stake-amount uint u100)
(define-data-var price-cooldown uint u144)
(define-data-var contract-paused bool false)

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
        paused: (var-get contract-paused)
    }
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
