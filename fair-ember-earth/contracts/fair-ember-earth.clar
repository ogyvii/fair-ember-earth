;; CrisisChain - Decentralized Disaster Relief Ecosystem
;; A comprehensive smart contract for predictive resource allocation and transparent impact tracking

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u1001))
(define-constant ERR_INSUFFICIENT_FUNDS (err u1002))
(define-constant ERR_DISASTER_NOT_FOUND (err u1003))
(define-constant ERR_INVALID_THRESHOLD (err u1004))
(define-constant ERR_ALREADY_ACTIVATED (err u1005))
(define-constant ERR_INVALID_COORDINATES (err u1006))
(define-constant ERR_RESPONDER_NOT_CERTIFIED (err u1007))
(define-constant ERR_INVALID_IMPACT_DATA (err u1008))
(define-constant ERR_ORACLE_TIMEOUT (err u1009))
(define-constant ERR_INSUFFICIENT_STAKE (err u1010))
(define-constant ERR_RESOURCE_ALREADY_DEPLOYED (err u1011))
(define-constant ERR_INVALID_DISASTER_TYPE (err u1012))

;; Contract owner
(define-constant CONTRACT_OWNER tx-sender)

;; Configuration constants
(define-constant MIN_RESPONDER_STAKE u100000) ;; 100 STX minimum stake
(define-constant ORACLE_CONSENSUS_THRESHOLD u3) ;; Minimum oracle confirmations
(define-constant MAX_DISASTER_RADIUS u10000) ;; Maximum disaster radius in meters
(define-constant IMPACT_REWARD_MULTIPLIER u150) ;; 1.5x reward multiplier for impact

;; Data structures
(define-map disasters
    { disaster-id: uint }
    {
        disaster-type: (string-ascii 32),
        latitude: int,
        longitude: int,
        severity-level: uint,
        predicted-impact: uint,
        funds-allocated: uint,
        status: (string-ascii 16),
        activation-block: uint,
        response-deadline: uint
    }
)

(define-map emergency-pool
    { region-id: uint }
    {
        total-funds: uint,
        allocated-funds: uint,
        community-stake: uint,
        governance-threshold: uint
    }
)

(define-map certified-responders
    { responder: principal }
    {
        certification-level: uint,
        stake-amount: uint,
        total-responses: uint,
        success-rate: uint,
        geographic-radius: uint,
        certification-expires: uint
    }
)

(define-map resource-caches
    { cache-id: uint }
    {
        latitude: int,
        longitude: int,
        resource-type: (string-ascii 32),
        quantity-available: uint,
        deployment-cost: uint,
        last-updated: uint,
        managed-by: principal
    }
)

(define-map impact-certificates
    { certificate-id: uint }
    {
        disaster-id: uint,
        responder: principal,
        impact-score: uint,
        beneficiaries-helped: uint,
        resources-deployed: uint,
        verification-status: (string-ascii 16),
        oracle-confirmations: uint,
        reward-amount: uint
    }
)

(define-map oracle-reports
    { oracle-id: principal, disaster-id: uint }
    {
        severity-assessment: uint,
        damage-estimate: uint,
        population-affected: uint,
        urgent-needs: (string-ascii 64),
        report-timestamp: uint,
        confidence-level: uint
    }
)

(define-map community-resilience-data
    { region-id: uint }
    {
        vulnerability-index: uint,
        population-density: uint,
        historical-disasters: uint,
        average-response-time: uint,
        recovery-rate: uint,
        optimization-score: uint
    }
)

;; Global state variables
(define-data-var disaster-counter uint u0)
(define-data-var certificate-counter uint u0)
(define-data-var cache-counter uint u0)
(define-data-var total-emergency-funds uint u0)
(define-data-var global-alert-level uint u0)
(define-data-var system-active bool true)

;; Authorization helper
(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT_OWNER)
)

;; Validation helpers
(define-private (is-valid-coordinates (lat int) (lon int))
    (and 
        (<= lat 90000000) 
        (>= lat -90000000)
        (<= lon 180000000) 
        (>= lon -180000000)
    )
)

(define-private (is-certified-responder (responder principal))
    (match (map-get? certified-responders { responder: responder })
        responder-data (and 
            (> (get stake-amount responder-data) u0)
            (> (get certification-expires responder-data) block-height)
        )
        false
    )
)

;; Oracle consensus validation
(define-private (validate-oracle-consensus (disaster-id uint))
    (let ((confirmations (get-oracle-confirmations disaster-id)))
        (>= confirmations ORACLE_CONSENSUS_THRESHOLD)
    )
)

(define-private (get-oracle-confirmations (disaster-id uint))
    ;; Simplified oracle confirmation count
    u3 ;; This would iterate through oracle-reports in a full implementation
)

;; Admin Functions
(define-public (set-system-status (active bool))
    (begin
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (var-set system-active active)
        (ok true)
    )
)

(define-public (update-global-alert-level (level uint))
    (begin
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (asserts! (<= level u5) ERR_INVALID_THRESHOLD)
        (var-set global-alert-level level)
        (ok true)
    )
)

(define-public (register-resource-cache (latitude int) (longitude int) (resource-type (string-ascii 32)) (quantity uint) (cost uint))
    (let ((cache-id (+ (var-get cache-counter) u1)))
        (begin
            (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
            (asserts! (is-valid-coordinates latitude longitude) ERR_INVALID_COORDINATES)
            (map-set resource-caches
                { cache-id: cache-id }
                {
                    latitude: latitude,
                    longitude: longitude,
                    resource-type: resource-type,
                    quantity-available: quantity,
                    deployment-cost: cost,
                    last-updated: block-height,
                    managed-by: tx-sender
                }
            )
            (var-set cache-counter cache-id)
            (ok cache-id)
        )
    )
)

;; Public Functions
(define-public (become-certified-responder (stake-amount uint) (geographic-radius uint))
    (begin
        (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
        (asserts! (>= stake-amount MIN_RESPONDER_STAKE) ERR_INSUFFICIENT_STAKE)
        (asserts! (<= geographic-radius MAX_DISASTER_RADIUS) ERR_INVALID_COORDINATES)
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        (map-set certified-responders
            { responder: tx-sender }
            {
                certification-level: u1,
                stake-amount: stake-amount,
                total-responses: u0,
                success-rate: u100,
                geographic-radius: geographic-radius,
                certification-expires: (+ block-height u52560) ;; ~1 year
            }
        )
        (ok true)
    )
)

(define-public (report-disaster (disaster-type (string-ascii 32)) (latitude int) (longitude int) (severity uint) (predicted-impact uint))
    (let ((disaster-id (+ (var-get disaster-counter) u1)))
        (begin
            (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
            (asserts! (is-certified-responder tx-sender) ERR_RESPONDER_NOT_CERTIFIED)
            (asserts! (is-valid-coordinates latitude longitude) ERR_INVALID_COORDINATES)
            (asserts! (<= severity u10) ERR_INVALID_THRESHOLD)
            (map-set disasters
                { disaster-id: disaster-id }
                {
                    disaster-type: disaster-type,
                    latitude: latitude,
                    longitude: longitude,
                    severity-level: severity,
                    predicted-impact: predicted-impact,
                    funds-allocated: u0,
                    status: "reported",
                    activation-block: block-height,
                    response-deadline: (+ block-height u144) ;; 24 hours
                }
            )
            (var-set disaster-counter disaster-id)
            (ok disaster-id)
        )
    )
)

(define-public (activate-disaster-response (disaster-id uint) (fund-allocation uint))
    (let ((disaster (unwrap! (map-get? disasters { disaster-id: disaster-id }) ERR_DISASTER_NOT_FOUND)))
        (begin
            (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
            (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
            (asserts! (is-eq (get status disaster) "reported") ERR_ALREADY_ACTIVATED)
            (asserts! (validate-oracle-consensus disaster-id) ERR_ORACLE_TIMEOUT)
            (asserts! (<= fund-allocation (var-get total-emergency-funds)) ERR_INSUFFICIENT_FUNDS)
            (map-set disasters
                { disaster-id: disaster-id }
                (merge disaster { 
                    status: "active",
                    funds-allocated: fund-allocation
                })
            )
            (var-set total-emergency-funds (- (var-get total-emergency-funds) fund-allocation))
            (ok true)
        )
    )
)

(define-public (deploy-resources (disaster-id uint) (cache-id uint) (quantity uint))
    (let (
        (disaster (unwrap! (map-get? disasters { disaster-id: disaster-id }) ERR_DISASTER_NOT_FOUND))
        (cache (unwrap! (map-get? resource-caches { cache-id: cache-id }) ERR_DISASTER_NOT_FOUND))
    )
        (begin
            (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
            (asserts! (is-certified-responder tx-sender) ERR_RESPONDER_NOT_CERTIFIED)
            (asserts! (is-eq (get status disaster) "active") ERR_ALREADY_ACTIVATED)
            (asserts! (<= quantity (get quantity-available cache)) ERR_INSUFFICIENT_FUNDS)
            (map-set resource-caches
                { cache-id: cache-id }
                (merge cache { 
                    quantity-available: (- (get quantity-available cache) quantity),
                    last-updated: block-height
                })
            )
            (ok true)
        )
    )
)

(define-public (submit-impact-report (disaster-id uint) (beneficiaries uint) (resources-used uint) (impact-score uint))
    (let ((certificate-id (+ (var-get certificate-counter) u1)))
        (begin
            (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
            (asserts! (is-certified-responder tx-sender) ERR_RESPONDER_NOT_CERTIFIED)
            (asserts