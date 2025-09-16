;; DeFi Lending Protocol
;; Allows users to lend and borrow assets with dynamic interest rates
;; based on utilization rates, with liquidation mechanisms to manage risk

;; Define SIP-010 fungible token trait
;; Note: In a real deployment, you would use the actual SIP-010 contract address
;; This example uses a placeholder principal that should be replaced with the actual deployed contract
(define-trait token-interface
  (
    ;; Transfer from the caller to a new principal
    (transfer (uint principal principal (optional (buff 256))) (response bool uint))
    
    ;; Get the token balance of the specified principal
    (get-balance (principal) (response uint uint))
    
    ;; Get the total supply of the token
    (get-total-supply () (response uint uint))
    
    ;; Get the token name
    (get-name () (response (string-ascii 32) uint))
    
    ;; Get the token symbol
    (get-symbol () (response (string-ascii 32) uint))
    
    ;; Get the token decimals
    (get-decimals () (response uint uint))
    
    ;; Get the URI for token metadata
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

;; Contract owner
(define-data-var system-administrator principal tx-sender)

;; Constants
(define-constant ERR-NOT-AUTHORIZED u1000)
(define-constant ERR-INVALID-PARAMETER u1001)
(define-constant ERR-RESOURCE-NOT-FOUND u1002)
(define-constant ERR-POOL-NOT-ACTIVE u1003)
(define-constant ERR-INVALID-TOKEN-CONTRACT u1004)
(define-constant ERR-AMOUNT-ZERO u1005)
(define-constant ERR-TRANSFER-FAILED u1006)
(define-constant ERR-INSUFFICIENT-BALANCE u1007)
(define-constant ERR-HEALTH-FACTOR-VIOLATION u1008)
(define-constant ERR-NO-BORROW-BALANCE u1009)
(define-constant ERR-NO-SUPPLY-BALANCE u1010)
(define-constant ERR-POSITION-NOT-LIQUIDATABLE u1011)
(define-constant ERR-RESOURCE-NOT-COLLATERAL u1012)
(define-constant ERR-INSUFFICIENT-COLLATERAL u1013)
(define-constant ERR-REPAY-AMOUNT-EXCEEDS-BORROW u1014)
(define-constant ERR-COLLATERAL-FACTOR-TOO-HIGH u1015)
(define-constant ERR-RESERVE-FACTOR-TOO-HIGH u1016)
(define-constant ERR-INVALID-LIQUIDATION-INCENTIVE u1017)
(define-constant ERR-INVALID-UTILIZATION u1018)

;; Supported asset details
(define-map available-resources
  { resource-id: uint }
  {
    resource-name: (string-ascii 32),
    contract-address: principal,
    operational: bool,
    backing-factor: uint,  ;; Out of 10000 (e.g., 7500 = 75%)
    protocol-factor: uint,     ;; Out of 10000 (e.g., 1000 = 10%)
    liquidation-bonus: uint,  ;; Out of 10000 (e.g., 1100 = 110%)
    minimum-rate: uint,          ;; Base interest rate (out of 10000)
    rate-multiplier: uint,         ;; Rate multiplier for utilization (out of 10000)
    surge-multiplier: uint,    ;; Jump multiplier after optimal utilization
    target-utilization: uint,;; Optimal utilization rate (out of 10000)
    aggregate-supplied: uint,
    aggregate-borrowed: uint,
    last-interest-timestamp: uint
  }
)

;; Market tokens (represents share of the lending pool)
(define-map pool-tokens
  { resource-id: uint }
  {
    token-name: (string-ascii 45),
    token-symbol: (string-ascii 33),
    token-decimals: uint,
    metadata-uri: (optional (string-utf8 256)),
    circulating-supply: uint
  }
)

;; User balances of market tokens
(define-map account-deposits
  { resource-id: uint, account: principal }
  {
    deposit-balance: uint,  ;; Market token balance
    used-as-collateral: bool  ;; Whether this asset is used as collateral
  }
)

;; User borrows
(define-map account-loans
  { resource-id: uint, account: principal }
  {
    loan-balance: uint,  ;; Amount borrowed plus accrued interest
    rate-index: uint  ;; Interest index at time of borrow
  }
)

;; Interest rate model data
(define-map rate-model-data
  { resource-id: uint }
  {
    cumulative-index: uint,  ;; Cumulative interest index (starts at 1e18)
    current-rate: uint,  ;; Current interest rate per block (out of 10000)
    last-calculation-timestamp: uint  ;; Last timestamp interest was accrued
  }
)

;; Price oracles for assets
(define-map pricing-oracles
  { resource-id: uint }
  {
    oracle-address: principal,
    decimal-scaling-factor: uint,  ;; To normalize decimal places
    cached-price: uint,
    price-update-timestamp: uint
  }
)

;; Protocol configuration
(define-data-var system-fee-recipient principal tx-sender)
(define-data-var liquidation-penalty-percentage uint u300)  ;; 3%
(define-data-var system-fee-percentage uint u1000)    ;; 10%
(define-data-var minimum-health-factor uint u10000)         ;; 1.0 (scaled by 10000)
(define-data-var liquidation-boundary uint u8500)      ;; 85%
(define-data-var next-resource-id uint u0)

;; Authorization check
(define-private (is-system-administrator)
  (is-eq tx-sender (var-get system-administrator))
)

;; Helper function to get the minimum of two uints
(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b)
)

;; Validate asset ID
(define-private (is-valid-resource-id (resource-id uint))
  (is-some (map-get? available-resources { resource-id: resource-id }))
)

;; Validate uint is within range
(define-private (is-within-range (value uint) (min uint) (max uint))
  (and (>= value min) (<= value max))
)

;; Validate string length
(define-private (is-valid-string-length (str (string-ascii 32)) (min uint) (max uint))
  (<= (len str) max)
)

;; Initialize a new lending market for an asset
(define-public (initialize-pool
                (contract-address principal)
                (resource-name (string-ascii 32))
                (backing-factor uint)
                (protocol-factor uint)
                (liquidation-bonus uint)
                (minimum-rate uint)
                (rate-multiplier uint)
                (surge-multiplier uint)
                (target-utilization uint))
  (begin
    ;; Authorize
    (asserts! (is-system-administrator) (err ERR-NOT-AUTHORIZED))
    
    ;; Validate parameters
    (asserts! (is-valid-string-length resource-name u1 u32) (err ERR-INVALID-PARAMETER))
    (asserts! (not (is-eq contract-address (as-contract tx-sender))) (err ERR-INVALID-PARAMETER))
    (asserts! (< backing-factor u10000) (err ERR-COLLATERAL-FACTOR-TOO-HIGH))
    (asserts! (< protocol-factor u5000) (err ERR-RESERVE-FACTOR-TOO-HIGH))
    (asserts! (> liquidation-bonus u10000) (err ERR-INVALID-LIQUIDATION-INCENTIVE))
    (asserts! (< target-utilization u10000) (err ERR-INVALID-UTILIZATION))
    (asserts! (is-within-range minimum-rate u0 u10000) (err ERR-INVALID-PARAMETER))
    (asserts! (is-within-range rate-multiplier u0 u100000) (err ERR-INVALID-PARAMETER))
    (asserts! (is-within-range surge-multiplier u0 u1000000) (err ERR-INVALID-PARAMETER))
    
    (let
      ((resource-id (var-get next-resource-id))
       (validated-name (if (> (len resource-name) u0) resource-name "Unnamed Asset")))
      
      ;; Create the asset record
      (map-set available-resources
        { resource-id: resource-id }
        {
          resource-name: validated-name,
          contract-address: contract-address,
          operational: true,
          backing-factor: backing-factor,
          protocol-factor: protocol-factor,
          liquidation-bonus: liquidation-bonus,
          minimum-rate: minimum-rate,
          rate-multiplier: rate-multiplier,
          surge-multiplier: surge-multiplier,
          target-utilization: target-utilization,
          aggregate-supplied: u0,
          aggregate-borrowed: u0,
          last-interest-timestamp: block-height
        }
      )
      
      ;; Initialize market tokens
      (map-set pool-tokens
        { resource-id: resource-id }
        {
          token-name: (concat validated-name " Market Token"),
          token-symbol: (concat "m" validated-name),
          token-decimals: u8,
          metadata-uri: none,
          circulating-supply: u0
        }
      )
      
      ;; Initialize interest rate model
      (map-set rate-model-data
        { resource-id: resource-id }
        {
          cumulative-index: u1000000000000000000,  ;; 1e18
          current-rate: minimum-rate,
          last-calculation-timestamp: block-height
        }
      )
      
      ;; Increment asset ID counter
      (var-set next-resource-id (+ resource-id u1))
      
      (ok resource-id)
    )
  )
)

;; Supply assets to the protocol
(define-public (deposit-resource (resource-id uint) (contract-address <token-interface>) (quantity uint))
  (begin
    ;; Validate asset ID
    (asserts! (is-valid-resource-id resource-id) (err ERR-RESOURCE-NOT-FOUND))
    
    (let
      ((resource (unwrap! (map-get? available-resources { resource-id: resource-id }) (err ERR-RESOURCE-NOT-FOUND)))
       (pool-token (unwrap! (map-get? pool-tokens { resource-id: resource-id }) (err ERR-RESOURCE-NOT-FOUND)))
       (deposit-balance (default-to { deposit-balance: u0, used-as-collateral: false }
                         (map-get? account-deposits { resource-id: resource-id, account: tx-sender }))))
      
      ;; Validate
      (asserts! (get operational resource) (err ERR-POOL-NOT-ACTIVE))
      (asserts! (is-eq (contract-of contract-address) (get contract-address resource)) (err ERR-INVALID-TOKEN-CONTRACT))
      (asserts! (> quantity u0) (err ERR-AMOUNT-ZERO))
      
      ;; Accrue interest
      (try! (calculate-interest resource-id))
      
      ;; Calculate market tokens to mint
      (let
        ((conversion-rate (calculate-conversion-rate resource-id))
         (mint-quantity (if (is-eq (get circulating-supply pool-token) u0)
                         quantity
                         (/ (* quantity u1000000000000000000) conversion-rate))))
        
        ;; Transfer tokens from user to protocol
        ;; Unwrap the result to standardize the error handling
        (unwrap! (contract-call? contract-address transfer quantity tx-sender (as-contract tx-sender) none) (err ERR-TRANSFER-FAILED))
        
        ;; Update market state
        (map-set available-resources
          { resource-id: resource-id }
          (merge resource { aggregate-supplied: (+ (get aggregate-supplied resource) quantity) })
        )
        
        ;; Update market token supply
        (map-set pool-tokens
          { resource-id: resource-id }
          (merge pool-token { circulating-supply: (+ (get circulating-supply pool-token) mint-quantity) })
        )
        
        ;; Update user supply balance
        (map-set account-deposits
          { resource-id: resource-id, account: tx-sender }
          {
            deposit-balance: (+ (get deposit-balance deposit-balance) mint-quantity),
            used-as-collateral: true
          }
        )
        
        (ok mint-quantity)
      )
    )
  )
)

;; Withdraw supplied assets
(define-public (withdraw-resource (resource-id uint) (contract-address <token-interface>) (quantity uint))
  (begin
    ;; Validate asset ID
    (asserts! (is-valid-resource-id resource-id) (err ERR-RESOURCE-NOT-FOUND))
    
    (let
      ((resource (unwrap! (map-get? available-resources { resource-id: resource-id }) (err ERR-RESOURCE-NOT-FOUND)))
       (pool-token (unwrap! (map-get? pool-tokens { resource-id: resource-id }) (err ERR-RESOURCE-NOT-FOUND)))
       (deposit-balance (unwrap! (map-get? account-deposits { resource-id: resource-id, account: tx-sender })
                               (err ERR-NO-SUPPLY-BALANCE))))
      
      ;; Validate
      (asserts! (get operational resource) (err ERR-POOL-NOT-ACTIVE))
      (asserts! (is-eq (contract-of contract-address) (get contract-address resource)) (err ERR-INVALID-TOKEN-CONTRACT))
      (asserts! (> quantity u0) (err ERR-AMOUNT-ZERO))
      
      ;; Accrue interest
      (try! (calculate-interest resource-id))
      
      ;; Calculate tokens to withdraw
      (let
        ((conversion-rate (calculate-conversion-rate resource-id))
         (tokens-to-burn (/ (* quantity u1000000000000000000) conversion-rate)))
        
        ;; Validate sufficient balance
        (asserts! (<= tokens-to-burn (get deposit-balance deposit-balance)) (err ERR-INSUFFICIENT-BALANCE))
        
        ;; Check that withdrawal maintains health factor
        (asserts! (or (not (get used-as-collateral deposit-balance))
                     (>= (get-wellness-factor tx-sender) (var-get minimum-health-factor)))
                  (err ERR-HEALTH-FACTOR-VIOLATION))
        
        ;; Update market state
        (map-set available-resources
          { resource-id: resource-id }
          (merge resource { aggregate-supplied: (- (get aggregate-supplied resource) quantity) })
        )
        
        ;; Update market token supply
        (map-set pool-tokens
          { resource-id: resource-id }
          (merge pool-token { circulating-supply: (- (get circulating-supply pool-token) tokens-to-burn) })
        )
        
        ;; Update user supply balance
        (map-set account-deposits
          { resource-id: resource-id, account: tx-sender }
          {
            deposit-balance: (- (get deposit-balance deposit-balance) tokens-to-burn),
            used-as-collateral: (get used-as-collateral deposit-balance)
          }
        )
        
        ;; Transfer tokens to user
        (as-contract (unwrap! (contract-call? contract-address transfer quantity tx-sender tx-sender none) (err ERR-TRANSFER-FAILED)))
        
        (ok quantity)
      )
    )
  )
)

;; Toggle whether an asset is used as collateral
(define-public (toggle-backing (resource-id uint))
  (begin
    ;; Validate asset ID
    (asserts! (is-valid-resource-id resource-id) (err ERR-RESOURCE-NOT-FOUND))
    
    (let
      ((deposit-balance (unwrap! (map-get? account-deposits { resource-id: resource-id, account: tx-sender })
                               (err ERR-NO-SUPPLY-BALANCE)))
       (current-status (get used-as-collateral deposit-balance)))
      
      ;; If turning off collateral, check if it would violate health factor
      (if (and current-status (> (get deposit-balance deposit-balance) u0))
          (asserts! (>= (simulate-wellness-factor-without-backing tx-sender resource-id)
                          (var-get minimum-health-factor))
                    (err ERR-HEALTH-FACTOR-VIOLATION))
          true)
      
      ;; Update collateral status
      (map-set account-deposits
        { resource-id: resource-id, account: tx-sender }
        (merge deposit-balance { used-as-collateral: (not current-status) })
      )
      
      (ok (not current-status))
    )
  )
)

;; Borrow assets from the protocol
(define-public (borrow-resource (resource-id uint) (contract-address <token-interface>) (quantity uint))
  (begin
    ;; Validate asset ID
    (asserts! (is-valid-resource-id resource-id) (err ERR-RESOURCE-NOT-FOUND))
    
    (let
      ((resource (unwrap! (map-get? available-resources { resource-id: resource-id }) (err ERR-RESOURCE-NOT-FOUND)))
       (rate-data (unwrap! (map-get? rate-model-data { resource-id: resource-id })
                               (err ERR-RESOURCE-NOT-FOUND)))
       (loan-balance (default-to { loan-balance: u0, rate-index: (get cumulative-index rate-data) }
                       (map-get? account-loans { resource-id: resource-id, account: tx-sender })))
       (borrow-quantity quantity))
      
      ;; Validate
      (asserts! (get operational resource) (err ERR-POOL-NOT-ACTIVE))
      (asserts! (is-eq (contract-of contract-address) (get contract-address resource)) (err ERR-INVALID-TOKEN-CONTRACT))
      (asserts! (> quantity u0) (err ERR-AMOUNT-ZERO))
      (asserts! (<= quantity (get-lending-capacity tx-sender resource-id))
                 (err ERR-INSUFFICIENT-COLLATERAL))
      
      ;; Accrue interest
      (try! (calculate-interest resource-id))
      
      ;; Update borrow balance with accrued interest
      (let
        ((accrued-balance (/ (* (get loan-balance loan-balance) (get cumulative-index rate-data))
                            (get rate-index loan-balance)))
         (new-balance (+ accrued-balance borrow-quantity)))
        
        ;; Update user borrow balance
        (map-set account-loans
          { resource-id: resource-id, account: tx-sender }
          {
            loan-balance: new-balance,
            rate-index: (get cumulative-index rate-data)
          }
        )
        
        ;; Update market state
        (map-set available-resources
          { resource-id: resource-id }
          (merge resource { aggregate-borrowed: (+ (get aggregate-borrowed resource) borrow-quantity) })
        )
        
        ;; Check health factor after borrow
        (asserts! (>= (get-wellness-factor tx-sender) (var-get minimum-health-factor))
                  (err ERR-HEALTH-FACTOR-VIOLATION))
        
        ;; Transfer tokens to borrower
        (as-contract (unwrap! (contract-call? contract-address transfer borrow-quantity tx-sender tx-sender none) (err ERR-TRANSFER-FAILED)))
        
        (ok borrow-quantity)
      )
    )
  )
)

;; Repay borrowed assets
(define-public (repay-loan (resource-id uint) (contract-address <token-interface>) (quantity uint))
  (begin
    ;; Validate asset ID
    (asserts! (is-valid-resource-id resource-id) (err ERR-RESOURCE-NOT-FOUND))
    
    (let
      ((resource (unwrap! (map-get? available-resources { resource-id: resource-id }) (err ERR-RESOURCE-NOT-FOUND)))
       (rate-data (unwrap! (map-get? rate-model-data { resource-id: resource-id })
                               (err ERR-RESOURCE-NOT-FOUND)))
       (loan-balance (unwrap! (map-get? account-loans { resource-id: resource-id, account: tx-sender })
                               (err ERR-NO-BORROW-BALANCE))))
      
      ;; Validate
      (asserts! (get operational resource) (err ERR-POOL-NOT-ACTIVE))
      (asserts! (is-eq (contract-of contract-address) (get contract-address resource)) (err ERR-INVALID-TOKEN-CONTRACT))
      (asserts! (> quantity u0) (err ERR-AMOUNT-ZERO))
      
      ;; Accrue interest
      (try! (calculate-interest resource-id))
      
      ;; Calculate current borrow balance with accrued interest
      (let
        ((current-loan-balance (/ (* (get loan-balance loan-balance) (get cumulative-index rate-data))
                                   (get rate-index loan-balance)))
         (repay-quantity (min-uint quantity current-loan-balance)))
        
        ;; Transfer tokens from user to protocol
        (unwrap! (contract-call? contract-address transfer repay-quantity tx-sender (as-contract tx-sender) none) (err ERR-TRANSFER-FAILED))
        
        ;; Update user borrow balance
        (map-set account-loans
          { resource-id: resource-id, account: tx-sender }
          {
            loan-balance: (- current-loan-balance repay-quantity),
            rate-index: (get cumulative-index rate-data)
          }
        )
        
        ;; Update market state
        (map-set available-resources
          { resource-id: resource-id }
          (merge resource { aggregate-borrowed: (- (get aggregate-borrowed resource) repay-quantity) })
        )
        
        (ok repay-quantity)
      )
    )
  )
)

;; Liquidate an unhealthy position
(define-public (liquidate-position
                 (debtor principal)
                 (repay-resource-id uint)
                 (repay-contract <token-interface>)
                (backing-resource-id uint)
                (repay-quantity uint))
  (begin
    ;; Validate asset IDs
    (asserts! (is-valid-resource-id repay-resource-id) (err ERR-RESOURCE-NOT-FOUND))
    (asserts! (is-valid-resource-id backing-resource-id) (err ERR-RESOURCE-NOT-FOUND))
    (asserts! (not (is-eq debtor tx-sender)) (err ERR-INVALID-PARAMETER))
    (asserts! (> repay-quantity u0) (err ERR-AMOUNT-ZERO))
    
    (let
      ((debtor-wellness (get-wellness-factor debtor))
       (repay-resource (unwrap! (map-get? available-resources { resource-id: repay-resource-id })
                             (err ERR-RESOURCE-NOT-FOUND)))
       (backing-resource (unwrap! (map-get? available-resources { resource-id: backing-resource-id })
                                  (err ERR-RESOURCE-NOT-FOUND)))
       (loan-balance (unwrap! (map-get? account-loans { resource-id: repay-resource-id, account: debtor })
                               (err ERR-NO-BORROW-BALANCE)))
       (backing-balance (unwrap! (map-get? account-deposits
                                    { resource-id: backing-resource-id, account: debtor })
                                  (err ERR-NO-SUPPLY-BALANCE))))
      
      ;; Validate
      (asserts! (< debtor-wellness (var-get minimum-health-factor))
                 (err ERR-POSITION-NOT-LIQUIDATABLE))
      (asserts! (get used-as-collateral backing-balance)
                 (err ERR-RESOURCE-NOT-COLLATERAL))
      (asserts! (is-eq (contract-of repay-contract) (get contract-address repay-resource))
                 (err ERR-INVALID-TOKEN-CONTRACT))
      
      ;; Accrue interest for both assets
      (try! (calculate-interest repay-resource-id))
      (try! (calculate-interest backing-resource-id))
      
      ;; Calculate current borrow balance with accrued interest
      (let
        ((rate-data (unwrap-panic (map-get? rate-model-data { resource-id: repay-resource-id })))
         (current-loan-balance (/ (* (get loan-balance loan-balance) (get cumulative-index rate-data))
                                   (get rate-index loan-balance)))
         (max-repay (/ (* current-loan-balance u5000) u10000))  ;; 50% of the debt
         (actual-repay-quantity (min-uint repay-quantity max-repay))
         
         ;; Calculate collateral to seize
         (repay-price (get-resource-price repay-resource-id))
         (backing-price (get-resource-price backing-resource-id))
         (liquidation-bonus (get liquidation-bonus backing-resource))
         (backing-quantity (/ (* (* actual-repay-quantity repay-price) liquidation-bonus)
                              (* backing-price u10000)))
         
         ;; Convert collateral amount to market tokens
         (conversion-rate (calculate-conversion-rate backing-resource-id))
         (pool-tokens-to-seize (/ (* backing-quantity u1000000000000000000) conversion-rate)))
        
        ;; Validate sufficient balances
        (asserts! (<= actual-repay-quantity current-loan-balance)
                   (err ERR-REPAY-AMOUNT-EXCEEDS-BORROW))
        (asserts! (<= pool-tokens-to-seize (get deposit-balance backing-balance))
                   (err ERR-INSUFFICIENT-COLLATERAL))
        
        ;; Transfer repay tokens from liquidator to protocol
        (unwrap! (contract-call? repay-contract transfer actual-repay-quantity tx-sender (as-contract tx-sender) none) (err ERR-TRANSFER-FAILED))
        
        ;; Update borrower's borrow balance
        (map-set account-loans
          { resource-id: repay-resource-id, account: debtor }
          {
            loan-balance: (- current-loan-balance actual-repay-quantity),
            rate-index: (get cumulative-index rate-data)
          }
        )
        
        ;; Update borrower's collateral balance
        (map-set account-deposits
          { resource-id: backing-resource-id, account: debtor }
          {
            deposit-balance: (- (get deposit-balance backing-balance) pool-tokens-to-seize),
            used-as-collateral: (get used-as-collateral backing-balance)
          }
        )
        
        ;; Update liquidator's collateral balance
        (let
          ((liquidator-balance (default-to { deposit-balance: u0, used-as-collateral: true }
                               (map-get? account-deposits
                                 { resource-id: backing-resource-id, account: tx-sender }))))
          
          (map-set account-deposits
            { resource-id: backing-resource-id, account: tx-sender }
            {
              deposit-balance: (+ (get deposit-balance liquidator-balance) pool-tokens-to-seize),
              used-as-collateral: (get used-as-collateral liquidator-balance)
            }
          )
        )
        
        ;; Update market state
        (map-set available-resources
          { resource-id: repay-resource-id }
          (merge repay-resource { aggregate-borrowed: (- (get aggregate-borrowed repay-resource) actual-repay-quantity) })
        )
        
        (ok actual-repay-quantity)
      )
    )
  )
)

;; Accrue interest for an asset
(define-public (calculate-interest (resource-id uint))
  (begin
    ;; Validate asset ID
    (asserts! (is-valid-resource-id resource-id) (err ERR-RESOURCE-NOT-FOUND))
    
    (let
      ((resource (unwrap! (map-get? available-resources { resource-id: resource-id }) (err ERR-RESOURCE-NOT-FOUND)))
       (rate-data (unwrap! (map-get? rate-model-data { resource-id: resource-id })
                               (err ERR-RESOURCE-NOT-FOUND)))
       (blocks-elapsed (- block-height (get last-calculation-timestamp rate-data)))
       (usage-rate (calculate-usage-rate resource-id))
       (current-rate (calculate-lending-rate resource-id usage-rate))
       (interest-accumulated (* (* blocks-elapsed current-rate) (get aggregate-borrowed resource)))
       (new-total-borrows (+ (get aggregate-borrowed resource) interest-accumulated))
       (new-cumulative-index (/ (* (get cumulative-index rate-data)
                              (+ u1000000000000000000 (* blocks-elapsed current-rate)))
                          u1000000000000000000))
       (protocol-amount (/ (* interest-accumulated (get protocol-factor resource)) u10000)))
      
      ;; Update interest rate data
      (map-set rate-model-data
        { resource-id: resource-id }
        {
          cumulative-index: new-cumulative-index,
          current-rate: current-rate,
          last-calculation-timestamp: block-height
        }
      )
      
      ;; Update market state
      (map-set available-resources
        { resource-id: resource-id }
        (merge resource
          {
            aggregate-borrowed: new-total-borrows,
            aggregate-supplied: (+ (get aggregate-supplied resource) protocol-amount),
            last-interest-timestamp: block-height
          }
        )
      )
      
      (ok current-rate)
    )
  )
)

;; Calculate the current utilization rate
(define-private (calculate-usage-rate (resource-id uint))
  (let
    ((resource (unwrap-panic (map-get? available-resources { resource-id: resource-id }))))
    
    (if (is-eq (get aggregate-supplied resource) u0)
        u0
        (/ (* (get aggregate-borrowed resource) u10000) (get aggregate-supplied resource))
    )
  )
)

;; Calculate the current borrow rate based on utilization
(define-private (calculate-lending-rate (resource-id uint) (usage-rate uint))
  (let
    ((resource (unwrap-panic (map-get? available-resources { resource-id: resource-id }))))
    
    (if (<= usage-rate (get target-utilization resource))
        ;; Below optimal: base-rate + utilization * multiplier
        (+ (get minimum-rate resource)
           (/ (* usage-rate (get rate-multiplier resource)) u10000))
        ;; Above optimal: base-rate + optimal*multiplier + (utilization-optimal) * jump-multiplier
        (+ (+ (get minimum-rate resource)
               (/ (* (get target-utilization resource) (get rate-multiplier resource)) u10000))
          (/ (* (- usage-rate (get target-utilization resource)) (get surge-multiplier resource)) u10000))
    )
  )
)

;; Calculate the current exchange rate for market tokens
(define-private (calculate-conversion-rate (resource-id uint))
  (let
    ((resource (unwrap-panic (map-get? available-resources { resource-id: resource-id })))
     (pool-token (unwrap-panic (map-get? pool-tokens { resource-id: resource-id }))))
    
    (if (is-eq (get circulating-supply pool-token) u0)
        u1000000000000000000  ;; 1:1 initially
        (/ (* (get aggregate-supplied resource) u1000000000000000000) (get circulating-supply pool-token))
    )
  )
)

;; Get current price of an asset from oracle
(define-private (get-resource-price (resource-id uint))
  ;; In a real implementation, this would call the oracle contract
  ;; Simplified for this example
  u1000000  ;; $1.00 with 6 decimals
)

;; Calculate the total collateral value for a user
(define-private (get-total-backing-value (account principal))
  ;; In a real implementation, this would iterate over all assets
  ;; Simplified for this example
  u1000000000  ;; $1000 with 6 decimals
)

;; Calculate the total borrow value for a user
(define-private (get-total-loan-value (account principal))
  ;; In a real implementation, this would iterate over all assets
  ;; Simplified for this example
  u500000000  ;; $500 with 6 decimals
)

;; Calculate a user's health factor
(define-read-only (get-wellness-factor (account principal))
  (let
    ((total-backing (get-total-backing-value account))
     (total-loans (get-total-loan-value account)))
    
    (if (is-eq total-loans u0)
        u1000000000000000000  ;; Maximum health if no borrows
        (/ (* total-backing (var-get liquidation-boundary)) (* total-loans u10000))
    )
  )
)

;; Simulate health factor if collateral is removed
(define-private (simulate-wellness-factor-without-backing (account principal) (resource-id uint))
  ;; In a real implementation, this would recalculate health without the specified collateral
  ;; Simplified for this example
  u10000  ;; 1.0 with 4 decimal scaling
)

;; Calculate borrowing capacity for a user
(define-private (get-lending-capacity (account principal) (resource-id uint))
  ;; In a real implementation, this would calculate based on collateral value and factor
  ;; Simplified for this example
  u1000000000  ;; 1000 tokens
)

;; Set the price oracle for an asset
(define-public (set-pricing-oracle
                 (resource-id uint)
                 (oracle-address principal)
                (decimal-scaling-factor uint))
  (begin
    ;; Authorize
    (asserts! (is-system-administrator) (err ERR-NOT-AUTHORIZED))
    
    ;; Validate asset ID
    (asserts! (is-valid-resource-id resource-id) (err ERR-RESOURCE-NOT-FOUND))
    (asserts! (> decimal-scaling-factor u0) (err ERR-INVALID-PARAMETER))
    
    (map-set pricing-oracles
      { resource-id: resource-id }
      {
        oracle-address: oracle-address,
        decimal-scaling-factor: decimal-scaling-factor,
        cached-price: u0,
        price-update-timestamp: u0
      }
    )
    
    (ok true)
  )
)

;; Update protocol parameters (only governance or admin)
(define-public (set-system-parameters
                (recipient principal)
                (liquidation-penalty uint)
                (system-fee uint)
                (minimum-wellness uint)
                (liquidation-limit uint))
  (begin
    ;; Authorize
    (asserts! (is-system-administrator) (err ERR-NOT-AUTHORIZED))
    
    ;; Validate parameters
    (asserts! (is-within-range liquidation-penalty u0 u5000) (err ERR-INVALID-PARAMETER))
    (asserts! (is-within-range system-fee u0 u5000) (err ERR-INVALID-PARAMETER))
    (asserts! (is-within-range minimum-wellness u5000 u20000) (err ERR-INVALID-PARAMETER))
    (asserts! (is-within-range liquidation-limit u5000 u10000) (err ERR-INVALID-PARAMETER))
    
    (var-set system-fee-recipient recipient)
    (var-set liquidation-penalty-percentage liquidation-penalty)
    (var-set system-fee-percentage system-fee)
    (var-set minimum-health-factor minimum-wellness)
    (var-set liquidation-boundary liquidation-limit)
    
    (ok true)
  )
)

;; Read-only functions
;; Get asset details
(define-read-only (get-resource-details (resource-id uint))
  (begin
    ;; Validate asset ID
    (asserts! (is-valid-resource-id resource-id) (err ERR-RESOURCE-NOT-FOUND))
    
    (ok (unwrap! (map-get? available-resources { resource-id: resource-id }) (err ERR-RESOURCE-NOT-FOUND)))
  )
)

;; Get user supply balance
(define-read-only (get-account-deposit (resource-id uint) (account principal))
  (begin
    ;; Validate asset ID
    (asserts! (is-valid-resource-id resource-id) (err ERR-RESOURCE-NOT-FOUND))
    
    (ok (default-to { deposit-balance: u0, used-as-collateral: false }
         (map-get? account-deposits { resource-id: resource-id, account: account })))
  )
)

;; Get user borrow balance
(define-read-only (get-account-loan (resource-id uint) (account principal))
  (begin
    ;; Validate asset ID
    (asserts! (is-valid-resource-id resource-id) (err ERR-RESOURCE-NOT-FOUND))
    
    (ok (default-to { loan-balance: u0, rate-index: u1000000000000000000 }
         (map-get? account-loans { resource-id: resource-id, account: account })))
  )
)

;; Get current interest rate
(define-read-only (get-current-rate (resource-id uint))
  (begin
    ;; Validate asset ID
    (asserts! (is-valid-resource-id resource-id) (err ERR-RESOURCE-NOT-FOUND))
    
    (ok (unwrap! (map-get? rate-model-data { resource-id: resource-id }) (err ERR-RESOURCE-NOT-FOUND)))
  )
)

;; Get market token details
(define-read-only (get-pool-token-details (resource-id uint))
  (begin
    ;; Validate asset ID
    (asserts! (is-valid-resource-id resource-id) (err ERR-RESOURCE-NOT-FOUND))
    
    (ok (unwrap! (map-get? pool-tokens { resource-id: resource-id }) (err ERR-RESOURCE-NOT-FOUND)))
  )
)

;; Get account liquidity
(define-read-only (get-account-liquidity (account principal))
  (let
    ((backing-value (get-total-backing-value account))
     (loan-value (get-total-loan-value account))
     (lending-capacity (/ (* backing-value (var-get liquidation-boundary)) u10000)))
    
    (if (> lending-capacity loan-value)
        ;; Excess liquidity
        (ok { excess: (- lending-capacity loan-value), shortfall: u0 })
        ;; Liquidity shortfall
        (ok { excess: u0, shortfall: (- loan-value lending-capacity) })
    )
  )
)