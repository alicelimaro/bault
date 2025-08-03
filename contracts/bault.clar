;; Multi-Asset Vault Contract with Security Enhancements
;; A comprehensive vault system supporting multiple assets with tiered fee structure

;; SIP-010 Trait Definition
(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

;; Error Constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PAUSED (err u101))
(define-constant ERR_NOT_WHITELISTED (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_INSUFFICIENT_SHARES (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_TRANSFER_FAILED (err u106))
(define-constant ERR_ASSET_NOT_FOUND (err u107))
(define-constant ERR_NOT_EMERGENCY (err u108))
(define-constant ERR_INVALID_TOKEN (err u1001))
(define-constant ERR_INVALID_RECIPIENT (err u1002))
(define-constant ERR_SELF_TRANSFER (err u1003))
(define-constant ERR_ASSET_EXISTS (err u1004))
(define-constant ERR_ASSET_DISABLED (err u1005))
(define-constant ERR_SLIPPAGE_EXCEEDED (err u1006))
(define-constant ERR_VAULT_INVARIANT (err u1007))

;; Contract Variables
(define-data-var admin principal tx-sender)
(define-data-var paused bool false)
(define-data-var emergency-mode bool false)
(define-data-var total-shares uint u0)
(define-data-var total-assets uint u0)
(define-data-var base-withdraw-fee-bps uint u50) ;; 0.5%
(define-data-var performance-fee-bps uint u200) ;; 2%
(define-data-var fee-recipient principal tx-sender)
(define-data-var staked-amount uint u0)

;; Data Maps
(define-map user-shares 
  {user: principal} 
  {
    shares: uint,
    total-volume: uint,
    last-deposit: uint
  }
)

(define-map whitelist 
  {user: principal} 
  {whitelisted: bool}
)

(define-map asset-configs 
  {token: principal} 
  {
    enabled: bool,
    max-allocation: uint,
    current-allocation: uint,
    decimals: uint
  }
)

(define-map asset-balances
  {token: principal}
  {balance: uint}
)

;; User Tier Configuration
(define-map user-tiers
  uint
  {
    min-volume: uint,
    withdraw-discount: uint,
    performance-discount: uint
  }
)

;; Default Values
(define-constant DEFAULT_USER_DATA {shares: u0, total-volume: u0, last-deposit: u0})

;; Input validation functions
(define-private (is-valid-amount (amount uint))
  (and (> amount u0) (<= amount u340282366920938463463374607431768211455))) ;; Max uint value

(define-private (is-valid-principal (addr principal))
  (not (is-eq addr 'SP000000000000000000002Q6VF78)))

(define-private (is-valid-token-contract (token <sip-010-trait>))
  (let ((token-principal (contract-of token)))
    (and 
      (is-valid-principal token-principal)
      (not (is-eq token-principal (as-contract tx-sender))))))

(define-private (validate-shares-amount (shares uint))
  (and 
    (> shares u0)
    (<= shares u340282366920938463463374607431768211455)))

(define-private (is-valid-fee-bps (fee-bps uint))
  (and (>= fee-bps u0) (<= fee-bps u10000))) ;; 0-100%

;; IMPROVEMENT 1: Division by Zero Protection and Vault Invariants
(define-private (check-vault-invariants)
  (let (
    (total-shares-val (var-get total-shares))
    (total-assets-val (var-get total-assets))
  )
    ;; Ensure mathematical consistency
    (and 
      (or (is-eq total-shares-val u0) (> total-assets-val u0))
      (or (is-eq total-assets-val u0) (> total-shares-val u0))
      ;; Additional invariant: if shares exist, assets must exist
      (or (is-eq total-shares-val u0) (> total-assets-val u0)))))

;; Helper Functions with Division by Zero Protection
(define-private (calculate-user-tier (user principal))
  (let ((user-data (default-to DEFAULT_USER_DATA (map-get? user-shares {user: user}))))
    (let ((volume (get total-volume user-data)))
      (if (>= volume u1000000000) u3 ;; Platinum: 1000+ STX
        (if (>= volume u500000000) u2 ;; Gold: 500+ STX
          (if (>= volume u100000000) u1 ;; Silver: 100+ STX
            u0)))))) ;; Basic: < 100 STX

(define-private (get-user-withdraw-fee (user principal) (amount uint))
  (let (
    (tier (calculate-user-tier user))
    (tier-info (default-to {min-volume: u0, withdraw-discount: u0, performance-discount: u0} (map-get? user-tiers tier)))
    (base-fee (var-get base-withdraw-fee-bps))
    (discount (get withdraw-discount tier-info))
    (discounted-fee (if (>= base-fee discount) (- base-fee discount) u0))
  )
    (/ (* amount discounted-fee) u10000)))

(define-private (get-user-performance-fee (user principal) (amount uint))
  (let (
    (tier (calculate-user-tier user))
    (tier-info (default-to {min-volume: u0, withdraw-discount: u0, performance-discount: u0} (map-get? user-tiers tier)))
    (base-fee (var-get performance-fee-bps))
    (discount (get performance-discount tier-info))
    (discounted-fee (if (>= base-fee discount) (- base-fee discount) u0))
  )
    (/ (* amount discounted-fee) u10000)))

;; IMPROVED: Safe calculation functions with division by zero protection
(define-private (calculate-shares (amount uint))
  (let (
    (current-total-shares (var-get total-shares))
    (current-total-assets (var-get total-assets))
  )
    (if (is-eq current-total-shares u0)
      amount ;; First deposit: 1:1 ratio
      (if (is-eq current-total-assets u0)
        u0 ;; Prevent division by zero - should not happen in normal operation
        (/ (* amount current-total-shares) current-total-assets)))))

(define-private (calculate-assets (shares uint))
  (let (
    (current-total-shares (var-get total-shares))
    (current-total-assets (var-get total-assets))
  )
    (if (or (is-eq current-total-shares u0) (is-eq shares u0))
      u0
      (/ (* shares current-total-assets) current-total-shares))))

;; IMPROVEMENT 2: Preview Functions for Slippage Protection
(define-read-only (preview-deposit (amount uint))
  (calculate-shares amount))

(define-read-only (preview-withdraw (shares uint))
  (calculate-assets shares))

(define-read-only (preview-withdraw-with-fee (user principal) (shares uint))
  (let (
    (assets-to-redeem (calculate-assets shares))
    (withdraw-fee (get-user-withdraw-fee user assets-to-redeem))
  )
    {
      gross-amount: assets-to-redeem,
      fee: withdraw-fee,
      net-amount: (if (>= assets-to-redeem withdraw-fee) (- assets-to-redeem withdraw-fee) u0)
    }))

;; Asset Management Functions
(define-public (add-asset (token <sip-010-trait>) (decimals uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    ;; Validate inputs
    (asserts! (is-valid-token-contract token) ERR_INVALID_TOKEN)
    (asserts! (and (>= decimals u0) (<= decimals u18)) ERR_INVALID_AMOUNT)
    
    (let ((token-principal (contract-of token)))
      (asserts! (is-none (map-get? asset-configs {token: token-principal})) ERR_ASSET_EXISTS)
      (map-set asset-configs 
        {token: token-principal}
        {
          enabled: true,
          max-allocation: u1000, ;; 10%
          current-allocation: u0,
          decimals: decimals
        })
      (map-set asset-balances {token: token-principal} {balance: u0})
      (ok true))))

(define-public (remove-asset (token <sip-010-trait>))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    (asserts! (is-valid-token-contract token) ERR_INVALID_TOKEN)
    
    (let ((token-principal (contract-of token)))
      (asserts! (is-some (map-get? asset-configs {token: token-principal})) ERR_ASSET_NOT_FOUND)
      (map-delete asset-configs {token: token-principal})
      (map-delete asset-balances {token: token-principal})
      (ok true))))

;; IMPROVED: Core Deposit Functions with Invariant Checks
(define-public (deposit (amount uint))
  (begin
    (asserts! (not (var-get paused)) ERR_PAUSED)
    (asserts! (is-some (map-get? whitelist {user: tx-sender})) ERR_NOT_WHITELISTED)
    (asserts! (is-valid-amount amount) ERR_INVALID_AMOUNT)
    (asserts! (check-vault-invariants) ERR_VAULT_INVARIANT) ;; Add invariant check
    
    (let (
      (shares-to-mint (calculate-shares amount))
      (user-data (default-to DEFAULT_USER_DATA (map-get? user-shares {user: tx-sender})))
      (current-shares (get shares user-data))
      (current-volume (get total-volume user-data))
    )
      ;; Transfer STX to contract
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      ;; Update user data
      (map-set user-shares 
        {user: tx-sender}
        {
          shares: (+ current-shares shares-to-mint),
          total-volume: (+ current-volume amount),
          last-deposit: stacks-block-height
        })
      
      ;; Update vault totals
      (var-set total-shares (+ (var-get total-shares) shares-to-mint))
      (var-set total-assets (+ (var-get total-assets) amount))
      
      (ok shares-to-mint))))

;; NEW: Enhanced deposit with slippage protection
(define-public (deposit-with-slippage (amount uint) (min-shares uint))
  (begin
    (asserts! (not (var-get paused)) ERR_PAUSED)
    (asserts! (is-some (map-get? whitelist {user: tx-sender})) ERR_NOT_WHITELISTED)
    (asserts! (is-valid-amount amount) ERR_INVALID_AMOUNT)
    (asserts! (> min-shares u0) ERR_INVALID_AMOUNT)
    (asserts! (check-vault-invariants) ERR_VAULT_INVARIANT)
    
    (let (
      (shares-to-mint (calculate-shares amount))
      (user-data (default-to DEFAULT_USER_DATA (map-get? user-shares {user: tx-sender})))
      (current-shares (get shares user-data))
      (current-volume (get total-volume user-data))
    )
      ;; Slippage protection
      (asserts! (>= shares-to-mint min-shares) ERR_SLIPPAGE_EXCEEDED)
      
      ;; Transfer STX to contract
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      ;; Update user data
      (map-set user-shares 
        {user: tx-sender}
        {
          shares: (+ current-shares shares-to-mint),
          total-volume: (+ current-volume amount),
          last-deposit: stacks-block-height
        })
      
      ;; Update vault totals
      (var-set total-shares (+ (var-get total-shares) shares-to-mint))
      (var-set total-assets (+ (var-get total-assets) amount))
      
      (ok shares-to-mint))))

(define-private (deposit-token-internal (token <sip-010-trait>) (amount uint))
  (let (
    (shares-to-mint (calculate-shares amount))
    (user-data (default-to DEFAULT_USER_DATA (map-get? user-shares {user: tx-sender})))
    (current-shares (get shares user-data))
    (current-volume (get total-volume user-data))
    (token-principal (contract-of token))
  )
    ;; Transfer token to contract
    (try! (contract-call? token transfer amount tx-sender (as-contract tx-sender) none))
    
    ;; Update asset balance
    (let ((current-balance (default-to {balance: u0} (map-get? asset-balances {token: token-principal}))))
      (map-set asset-balances 
        {token: token-principal} 
        {balance: (+ (get balance current-balance) amount)}))
    
    ;; Update user data
    (map-set user-shares 
      {user: tx-sender}
      {
        shares: (+ current-shares shares-to-mint),
        total-volume: (+ current-volume amount),
        last-deposit: stacks-block-height
      })
    
    ;; Update vault totals
    (var-set total-shares (+ (var-get total-shares) shares-to-mint))
    (var-set total-assets (+ (var-get total-assets) amount))
    
    ;; Return the shares minted wrapped in ok
    (ok shares-to-mint)))

(define-public (deposit-token (token <sip-010-trait>) (amount uint))
  (begin
    (asserts! (not (var-get paused)) ERR_PAUSED)
    (asserts! (is-some (map-get? whitelist {user: tx-sender})) ERR_NOT_WHITELISTED)
    ;; Validate inputs
    (asserts! (is-valid-token-contract token) ERR_INVALID_TOKEN)
    (asserts! (is-valid-amount amount) ERR_INVALID_AMOUNT)
    (asserts! (check-vault-invariants) ERR_VAULT_INVARIANT) ;; Add invariant check
    
    (let (
      (token-principal (contract-of token))
      (asset-config (unwrap! (map-get? asset-configs {token: token-principal}) ERR_ASSET_NOT_FOUND))
    )
      (asserts! (get enabled asset-config) ERR_ASSET_DISABLED)
      ;; Call internal function which returns a response
      (deposit-token-internal token amount))))

;; NEW: Enhanced token deposit with slippage protection
(define-public (deposit-token-with-slippage (token <sip-010-trait>) (amount uint) (min-shares uint))
  (begin
    (asserts! (not (var-get paused)) ERR_PAUSED)
    (asserts! (is-some (map-get? whitelist {user: tx-sender})) ERR_NOT_WHITELISTED)
    (asserts! (is-valid-token-contract token) ERR_INVALID_TOKEN)
    (asserts! (is-valid-amount amount) ERR_INVALID_AMOUNT)
    (asserts! (> min-shares u0) ERR_INVALID_AMOUNT)
    (asserts! (check-vault-invariants) ERR_VAULT_INVARIANT)
    
    (let (
      (token-principal (contract-of token))
      (asset-config (unwrap! (map-get? asset-configs {token: token-principal}) ERR_ASSET_NOT_FOUND))
      (shares-to-mint (calculate-shares amount))
    )
      (asserts! (get enabled asset-config) ERR_ASSET_DISABLED)
      ;; Slippage protection
      (asserts! (>= shares-to-mint min-shares) ERR_SLIPPAGE_EXCEEDED)
      
      (deposit-token-internal token amount))))

;; Core Withdrawal Functions
(define-private (withdraw-internal (user principal) (shares uint))
  (let (
    (assets-to-redeem (calculate-assets shares))
    (withdraw-fee (get-user-withdraw-fee user assets-to-redeem))
    (net-amount (if (>= assets-to-redeem withdraw-fee) (- assets-to-redeem withdraw-fee) u0))
    (user-data (unwrap-panic (map-get? user-shares {user: user})))
    (current-shares (get shares user-data))
    (current-volume (get total-volume user-data))
  )
    ;; Transfer fee to fee recipient (if any)
    (if (> withdraw-fee u0)
      (unwrap-panic (as-contract (stx-transfer? withdraw-fee tx-sender (var-get fee-recipient))))
      true)
    
    ;; Transfer net amount to user
    (unwrap-panic (as-contract (stx-transfer? net-amount tx-sender user)))
    
    ;; Update user shares
    (map-set user-shares 
      {user: user}
      {
        shares: (- current-shares shares),
        total-volume: current-volume,
        last-deposit: (get last-deposit user-data)
      })
    
    ;; Update vault totals
    (var-set total-shares (- (var-get total-shares) shares))
    (var-set total-assets (- (var-get total-assets) assets-to-redeem))
    
    ;; Return the net amount (not wrapped in ok)
    net-amount))

(define-public (withdraw (shares uint))
  (begin
    (asserts! (not (var-get paused)) ERR_PAUSED)
    (asserts! (is-some (map-get? whitelist {user: tx-sender})) ERR_NOT_WHITELISTED)
    ;; Validate input
    (asserts! (validate-shares-amount shares) ERR_INVALID_AMOUNT)
    (asserts! (check-vault-invariants) ERR_VAULT_INVARIANT) ;; Add invariant check
    
    (let (
      (user-data (default-to DEFAULT_USER_DATA (map-get? user-shares {user: tx-sender})))
      (user-shares-balance (get shares user-data))
    )
      (asserts! (>= user-shares-balance shares) ERR_INSUFFICIENT_SHARES)
      ;; Call internal function and wrap result in ok
      (ok (withdraw-internal tx-sender shares)))))

;; NEW: Enhanced withdraw with slippage protection
(define-public (withdraw-with-slippage (shares uint) (min-assets uint))
  (begin
    (asserts! (not (var-get paused)) ERR_PAUSED)
    (asserts! (is-some (map-get? whitelist {user: tx-sender})) ERR_NOT_WHITELISTED)
    (asserts! (validate-shares-amount shares) ERR_INVALID_AMOUNT)
    (asserts! (> min-assets u0) ERR_INVALID_AMOUNT)
    (asserts! (check-vault-invariants) ERR_VAULT_INVARIANT)
    
    (let (
      (user-data (default-to DEFAULT_USER_DATA (map-get? user-shares {user: tx-sender})))
      (user-shares-balance (get shares user-data))
      (assets-to-redeem (calculate-assets shares))
      (withdraw-fee (get-user-withdraw-fee tx-sender assets-to-redeem))
      (net-amount (if (>= assets-to-redeem withdraw-fee) (- assets-to-redeem withdraw-fee) u0))
    )
      (asserts! (>= user-shares-balance shares) ERR_INSUFFICIENT_SHARES)
      ;; Slippage protection
      (asserts! (>= net-amount min-assets) ERR_SLIPPAGE_EXCEEDED)
      
      (ok (withdraw-internal tx-sender shares)))))

(define-private (withdraw-token-internal (user principal) (shares uint))
  (let (
    (assets-to-redeem (calculate-assets shares))
    (withdraw-fee (get-user-withdraw-fee user assets-to-redeem))
    (net-amount (if (>= assets-to-redeem withdraw-fee) (- assets-to-redeem withdraw-fee) u0))
    (user-data (unwrap-panic (map-get? user-shares {user: user})))
    (current-shares (get shares user-data))
    (current-volume (get total-volume user-data))
  )
    ;; Update user shares
    (map-set user-shares 
      {user: user}
      {
        shares: (- current-shares shares),
        total-volume: current-volume,
        last-deposit: (get last-deposit user-data)
      })
    
    ;; Update vault totals
    (var-set total-shares (- (var-get total-shares) shares))
    (var-set total-assets (- (var-get total-assets) assets-to-redeem))
    
    ;; Return the net amount (not wrapped in ok)
    net-amount))

(define-public (withdraw-token (shares uint))
  (begin
    (asserts! (not (var-get paused)) ERR_PAUSED)
    (asserts! (is-some (map-get? whitelist {user: tx-sender})) ERR_NOT_WHITELISTED)
    ;; Validate input
    (asserts! (validate-shares-amount shares) ERR_INVALID_AMOUNT)
    (asserts! (check-vault-invariants) ERR_VAULT_INVARIANT) ;; Add invariant check
    
    (let (
      (user-data (default-to DEFAULT_USER_DATA (map-get? user-shares {user: tx-sender})))
      (user-shares-balance (get shares user-data))
    )
      (asserts! (>= user-shares-balance shares) ERR_INSUFFICIENT_SHARES)
      ;; Call internal function and wrap result in ok
      (ok (withdraw-token-internal tx-sender shares)))))

;; Emergency Functions
(define-private (emergency-withdraw-internal (user principal) (recipient principal))
  (let (
    (user-data (unwrap-panic (map-get? user-shares {user: user})))
    (user-shares-balance (get shares user-data))
    (assets-to-redeem (calculate-assets user-shares-balance))
  )
    ;; Transfer all assets to recipient (no fees in emergency)
    (unwrap-panic (as-contract (stx-transfer? assets-to-redeem tx-sender recipient)))
    
    ;; Clear user shares
    (map-set user-shares 
      {user: user}
      {
        shares: u0,
        total-volume: (get total-volume user-data),
        last-deposit: (get last-deposit user-data)
      })
    
    ;; Update vault totals
    (var-set total-shares (- (var-get total-shares) user-shares-balance))
    (var-set total-assets (- (var-get total-assets) assets-to-redeem))
    
    ;; Return the assets redeemed
    assets-to-redeem))

(define-public (emergency-withdraw (recipient principal))
  (begin
    (asserts! (var-get emergency-mode) ERR_NOT_EMERGENCY)
    ;; Validate input
    (asserts! (is-valid-principal recipient) ERR_INVALID_RECIPIENT)
    
    (let (
      (user-data (default-to DEFAULT_USER_DATA (map-get? user-shares {user: tx-sender})))
      (user-shares-balance (get shares user-data))
    )
      (asserts! (> user-shares-balance u0) ERR_INSUFFICIENT_SHARES)
      (ok (emergency-withdraw-internal tx-sender recipient)))))

;; Transfer Functions
(define-private (transfer-internal (from principal) (to principal) (shares uint))
  (let (
    (from-data (unwrap-panic (map-get? user-shares {user: from})))
    (to-data (default-to DEFAULT_USER_DATA (map-get? user-shares {user: to})))
    (from-shares (get shares from-data))
    (to-shares (get shares to-data))
  )
    ;; Update sender
    (map-set user-shares 
      {user: from}
      {
        shares: (- from-shares shares),
        total-volume: (get total-volume from-data),
        last-deposit: (get last-deposit from-data)
      })
    
    ;; Update recipient
    (map-set user-shares 
      {user: to}
      {
        shares: (+ to-shares shares),
        total-volume: (get total-volume to-data),
        last-deposit: (get last-deposit to-data)
      })
    
    ;; Return success
    true))

(define-public (transfer (to principal) (shares uint))
  (begin
    (asserts! (not (var-get paused)) ERR_PAUSED)
    ;; Validate inputs
    (asserts! (is-valid-principal to) ERR_INVALID_RECIPIENT)
    (asserts! (validate-shares-amount shares) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq tx-sender to)) ERR_SELF_TRANSFER)
    
    (let (
      (sender-data (default-to DEFAULT_USER_DATA (map-get? user-shares {user: tx-sender})))
      (sender-shares (get shares sender-data))
    )
      (asserts! (>= sender-shares shares) ERR_INSUFFICIENT_SHARES)
      (ok (transfer-internal tx-sender to shares)))))

;; Strategy Functions
(define-public (stake-into-strategy)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    (let ((available-balance (stx-get-balance (as-contract tx-sender))))
      (var-set staked-amount available-balance)
      (ok available-balance))))

(define-public (harvest-yield)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    ;; Simulate yield generation (10% of staked amount)
    (let (
      (current-staked (var-get staked-amount))
      (yield-amount (/ current-staked u10))
    )
      (var-set total-assets (+ (var-get total-assets) yield-amount))
      (ok yield-amount))))

;; Admin Functions
(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    (asserts! (is-valid-principal new-admin) ERR_INVALID_RECIPIENT)
    (var-set admin new-admin)
    (ok true)))

(define-public (pause)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    (var-set paused true)
    (ok true)))

(define-public (unpause)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    (var-set paused false)
    (ok true)))

(define-public (set-emergency-mode (enabled bool))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    (var-set emergency-mode enabled)
    (ok true)))

(define-public (set-base-withdraw-fee (fee-bps uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    (asserts! (is-valid-fee-bps fee-bps) ERR_INVALID_AMOUNT)
    (asserts! (<= fee-bps u1000) ERR_INVALID_AMOUNT) ;; Max 10%
    
    (var-set base-withdraw-fee-bps fee-bps)
    (ok true)))

(define-public (set-performance-fee (fee-bps uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    (asserts! (is-valid-fee-bps fee-bps) ERR_INVALID_AMOUNT)
    (asserts! (<= fee-bps u2000) ERR_INVALID_AMOUNT) ;; Max 20%
    
    (var-set performance-fee-bps fee-bps)
    (ok true)))

(define-public (set-fee-recipient (recipient principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    ;; Validate input
    (asserts! (is-valid-principal recipient) ERR_INVALID_RECIPIENT)
    
    (var-set fee-recipient recipient)
    (ok true)))

;; Whitelist Management
(define-public (add-to-whitelist (user principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    ;; Validate input
    (asserts! (is-valid-principal user) ERR_INVALID_RECIPIENT)
    
    (map-set whitelist {user: user} {whitelisted: true})
    (ok true)))

(define-public (remove-from-whitelist (user principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    (asserts! (is-valid-principal user) ERR_INVALID_RECIPIENT)
    
    (map-delete whitelist {user: user})
    (ok true)))

;; Read-Only Functions
(define-read-only (get-user-shares (user principal))
  (default-to DEFAULT_USER_DATA (map-get? user-shares {user: user})))

(define-read-only (get-user-tier (user principal))
  (calculate-user-tier user))

(define-read-only (get-vault-info)
  {
    total-shares: (var-get total-shares),
    total-assets: (var-get total-assets),
    share-price: (if (> (var-get total-shares) u0) 
                   (/ (* (var-get total-assets) u1000000) (var-get total-shares))
                   u1000000),
    paused: (var-get paused),
    emergency-mode: (var-get emergency-mode)
  })

(define-read-only (get-asset-config (token principal))
  (map-get? asset-configs {token: token}))

(define-read-only (get-asset-balance (token principal))
  (map-get? asset-balances {token: token}))

(define-read-only (is-whitelisted (user principal))
  (is-some (map-get? whitelist {user: user})))

(define-read-only (get-withdraw-fee-preview (user principal) (shares uint))
  (let ((assets (calculate-assets shares)))
    (get-user-withdraw-fee user assets)))

;; NEW: Vault health check function
(define-read-only (get-vault-health)
  {
    invariants-valid: (check-vault-invariants),
    total-shares: (var-get total-shares),
    total-assets: (var-get total-assets),
    share-asset-ratio: (if (> (var-get total-shares) u0)
                        (/ (* (var-get total-assets) u1000000) (var-get total-shares))
                        u0)
  })

;; Legacy compatibility functions
(define-public (get-balance (user principal))
  (ok (get shares (get-user-shares user))))

(define-public (get-total-supply)
  (ok (var-get total-shares)))

;; Initialize user tiers
(map-set user-tiers u0 {min-volume: u0, withdraw-discount: u0, performance-discount: u0})
(map-set user-tiers u1 {min-volume: u100000000, withdraw-discount: u10, performance-discount: u25}) ;; 100 STX
(map-set user-tiers u2 {min-volume: u500000000, withdraw-discount: u25, performance-discount: u50}) ;; 500 STX
(map-set user-tiers u3 {min-volume: u1000000000, withdraw-discount: u50, performance-discount: u100}) ;; 1000 STX

;; Initialize admin whitelist
(map-set whitelist {user: tx-sender} {whitelisted: true})
