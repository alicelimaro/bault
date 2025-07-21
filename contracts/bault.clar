;; --- Constants & Errors ---
(define-constant ERR-NO-FUNDS (err u100))
(define-constant ERR-NO-SHARES (err u101))
(define-constant ERR-PAUSED (err u102))
(define-constant ERR-NOT-ADMIN (err u103))
(define-constant ERR-INVALID-FEE (err u104))
(define-constant ERR-NOT-WHITELISTED (err u105))
(define-constant ERR-NOT-EMERGENCY (err u106))
(define-constant ERR-ASSET-NOT-SUPPORTED (err u107))
(define-constant ERR-ASSET-DISABLED (err u108))
(define-constant ERR-MAX-ALLOCATION-EXCEEDED (err u109))
(define-constant ERR-INVALID-TOKEN (err u110))

;; --- SIP-010 Trait Definition ---
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

;; --- Admin & Pause ---
(define-data-var admin principal tx-sender)
(define-data-var paused bool false)
(define-data-var emergency-mode bool false)

;; --- Vault State ---
(define-data-var total-shares uint u0)
(define-data-var total-assets uint u0)
(define-data-var strategy uint u0) ;; 0 = idle, 1 = basic yield, etc.

;; --- Multi-Asset Support ---
(define-map asset-configs 
  {token: principal} 
  {
    enabled: bool,
    max-allocation: uint, ;; in basis points (10000 = 100%)
    current-allocation: uint,
    decimals: uint
  }
)

(define-map asset-balances 
  {token: principal} 
  uint
)

;; --- User Data ---
(define-map shares {user: principal} uint)
(define-map user-tiers 
  {user: principal} 
  {
    tier: uint, ;; 0=basic, 1=silver, 2=gold, 3=platinum
    total-volume: uint,
    last-deposit-height: uint
  }
)

;; --- Legacy Support ---
(define-data-var staked-amount uint u0)
(define-data-var fake-yield uint u0) ;; simulate returns

;; --- Dynamic Fee Structure ---
(define-data-var base-withdraw-fee-bps uint u50) ;; 0.5% default (basis points: 1/10000)
(define-data-var performance-fee-bps uint u200) ;; 2% performance fee
(define-data-var fee-recipient principal tx-sender)

;; Tier-based fee discounts (in basis points reduction)
(define-map tier-discounts 
  {tier: uint} 
  {
    withdraw-discount: uint,
    performance-discount: uint,
    volume-threshold: uint ;; STX volume needed for tier
  }
)

;; --- Whitelist ---
(define-map whitelist {user: principal} bool)

;; --- Helper Functions ---
(define-private (only-admin)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-ADMIN)
    (ok true)
  )
)

(define-private (when-not-paused)
  (begin
    (asserts! (not (var-get paused)) ERR-PAUSED)
    (ok true)
  )
)

(define-private (only-whitelisted)
  (begin
    (asserts! (default-to false (map-get? whitelist {user: tx-sender})) ERR-NOT-WHITELISTED)
    (ok true)
  )
)

(define-private (is-asset-supported (token principal))
  (let ((config (map-get? asset-configs {token: token})))
    (and (is-some config) (get enabled (unwrap-panic config)))
  )
)

;; --- Initialize Default Assets ---
(define-private (init-default-assets)
  (begin
    ;; Add STX as default asset (using tx-sender as placeholder for STX)
    (map-set asset-configs 
      {token: tx-sender} 
      {enabled: true, max-allocation: u10000, current-allocation: u0, decimals: u6})
    
    ;; Initialize tier discounts
    (map-set tier-discounts {tier: u0} {withdraw-discount: u0, performance-discount: u0, volume-threshold: u0})
    (map-set tier-discounts {tier: u1} {withdraw-discount: u10, performance-discount: u25, volume-threshold: u100000000}) ;; 100 STX
    (map-set tier-discounts {tier: u2} {withdraw-discount: u25, performance-discount: u50, volume-threshold: u500000000}) ;; 500 STX
    (map-set tier-discounts {tier: u3} {withdraw-discount: u50, performance-discount: u100, volume-threshold: u1000000000}) ;; 1000 STX
    
    (ok true)
  )
)

;; Initialize on deployment
(init-default-assets)

;; --- Multi-Asset Management ---
(define-public (add-supported-asset (token <sip-010-trait>) (max-allocation uint) (decimals uint))
  (begin
    (try! (only-admin))
    (asserts! (<= max-allocation u10000) ERR-INVALID-FEE) ;; max 100%
    (map-set asset-configs 
      {token: (contract-of token)} 
      {enabled: true, max-allocation: max-allocation, current-allocation: u0, decimals: decimals})
    (print {event: "asset-added", token: (contract-of token), max-allocation: max-allocation})
    (ok true)
  )
)

(define-public (toggle-asset (token principal) (enabled bool))
  (begin
    (try! (only-admin))
    (let ((config (unwrap! (map-get? asset-configs {token: token}) ERR-ASSET-NOT-SUPPORTED)))
      (map-set asset-configs 
        {token: token} 
        (merge config {enabled: enabled}))
      (print {event: "asset-toggled", token: token, enabled: enabled})
      (ok true)
    )
  )
)

;; --- Dynamic Fee Calculation ---
(define-private (calculate-user-tier (user principal))
  (let ((user-data (default-to {tier: u0, total-volume: u0, last-deposit-height: u0} 
                               (map-get? user-tiers {user: user}))))
    (let ((volume (get total-volume user-data)))
      (if (>= volume u1000000000) u3 ;; Platinum: 1000+ STX
        (if (>= volume u500000000) u2 ;; Gold: 500+ STX
          (if (>= volume u100000000) u1 ;; Silver: 100+ STX
            u0))) ;; Basic: < 100 STX
    )
  )
)

(define-private (get-user-withdraw-fee (user principal) (amount uint))
  (let (
    (base-fee (var-get base-withdraw-fee-bps))
    (user-tier (calculate-user-tier user))
    (tier-info (default-to {withdraw-discount: u0, performance-discount: u0, volume-threshold: u0} 
                           (map-get? tier-discounts {tier: user-tier})))
    (discount (get withdraw-discount tier-info))
    (discounted-fee (if (>= base-fee discount) (- base-fee discount) u0))
  )
    (/ (* amount discounted-fee) u10000)
  )
)

(define-private (update-user-tier (user principal) (volume-to-add uint))
  (let (
    (current-data (default-to {tier: u0, total-volume: u0, last-deposit-height: stacks-block-height} 
                             (map-get? user-tiers {user: user})))
    (new-volume (+ (get total-volume current-data) volume-to-add))
    (new-tier (calculate-user-tier user))
  )
    (map-set user-tiers 
      {user: user} 
      {tier: new-tier, total-volume: new-volume, last-deposit-height: stacks-block-height})
    (ok new-tier)
  )
)

;; --- Admin Functions ---
(define-public (set-admin (new-admin principal))
  (begin
    (try! (only-admin))
    (var-set admin new-admin)
    (ok true)
  )
)

(define-public (pause)
  (begin
    (try! (only-admin))
    (var-set paused true)
    (print {event: "paused"})
    (ok true)
  )
)

(define-public (unpause)
  (begin
    (try! (only-admin))
    (var-set paused false)
    (print {event: "unpaused"})
    (ok true)
  )
)

(define-public (set-base-withdraw-fee (fee-bps uint))
  (begin
    (try! (only-admin))
    (asserts! (<= fee-bps u1000) ERR-INVALID-FEE) ;; max 10%
    (var-set base-withdraw-fee-bps fee-bps)
    (ok true)
  )
)

(define-public (set-performance-fee (fee-bps uint))
  (begin
    (try! (only-admin))
    (asserts! (<= fee-bps u2000) ERR-INVALID-FEE) ;; max 20%
    (var-set performance-fee-bps fee-bps)
    (ok true)
  )
)

(define-public (set-fee-recipient (recipient principal))
  (begin
    (try! (only-admin))
    (var-set fee-recipient recipient)
    (ok true)
  )
)

(define-public (update-tier-discount (tier uint) (withdraw-discount uint) (performance-discount uint) (volume-threshold uint))
  (begin
    (try! (only-admin))
    (asserts! (<= tier u3) ERR-INVALID-FEE)
    (asserts! (<= withdraw-discount u100) ERR-INVALID-FEE) ;; max 1% discount
    (asserts! (<= performance-discount u200) ERR-INVALID-FEE) ;; max 2% discount
    (map-set tier-discounts 
      {tier: tier} 
      {withdraw-discount: withdraw-discount, performance-discount: performance-discount, volume-threshold: volume-threshold})
    (ok true)
  )
)

(define-public (update-whitelist (user principal) (status bool))
  (begin
    (try! (only-admin))
    (map-set whitelist {user: user} status)
    (print {event: "whitelist-updated", user: user, status: status})
    (ok true)
  )
)

(define-public (enable-emergency)
  (begin
    (try! (only-admin))
    (var-set emergency-mode true)
    (print {event: "emergency-enabled"})
    (ok true)
  )
)

(define-public (disable-emergency)
  (begin
    (try! (only-admin))
    (var-set emergency-mode false)
    (print {event: "emergency-disabled"})
    (ok true)
  )
)

;; --- Upgradeability ---
(define-public (upgrade (new-contract principal))
  (begin
    (try! (only-admin))
    (print {event: "upgraded", new-contract: new-contract})
    (ok true)
  )
)

;; --- STX Deposit (Legacy Support) ---
(define-public (deposit (amount uint))
  (begin
    (try! (when-not-paused))
    (try! (only-whitelisted))
    (asserts! (> amount u0) ERR-NO-FUNDS)
    (let (
          (assets (var-get total-assets))
          (cur-total-shares (var-get total-shares))
          (new-shares (if (is-eq assets u0)
                            amount
                            (/ (* amount cur-total-shares) assets)))
      )
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (map-set shares {user: tx-sender} 
               (+ (default-to u0 (map-get? shares {user: tx-sender})) new-shares))
      (var-set total-assets (+ assets amount))
      (var-set total-shares (+ cur-total-shares new-shares))
      
      ;; Update user tier
      (unwrap-panic (update-user-tier tx-sender amount))
      
      (print {event: "deposit", user: tx-sender, amount: amount, shares: new-shares})
      (ok new-shares)
    )
  )
)

;; --- Multi-Asset Deposit ---
(define-public (deposit-token (token <sip-010-trait>) (amount uint))
  (begin
    (try! (when-not-paused))
    (try! (only-whitelisted))
    (asserts! (> amount u0) ERR-NO-FUNDS)
    (asserts! (is-asset-supported (contract-of token)) ERR-ASSET-NOT-SUPPORTED)
    
    (let (
      (token-principal (contract-of token))
      (assets (var-get total-assets))
      (cur-total-shares (var-get total-shares))
      ;; Convert token amount to STX equivalent (simplified 1:1 for now)
      (stx-equivalent amount)
      (new-shares (if (is-eq assets u0)
                        stx-equivalent
                        (/ (* stx-equivalent cur-total-shares) assets)))
    )
      ;; Transfer tokens to contract
      (try! (contract-call? token transfer amount tx-sender (as-contract tx-sender) none))
      
      ;; Update user shares
      (map-set shares {user: tx-sender} 
               (+ (default-to u0 (map-get? shares {user: tx-sender})) new-shares))
      
      ;; Update vault totals
      (var-set total-assets (+ assets stx-equivalent))
      (var-set total-shares (+ cur-total-shares new-shares))
      
      ;; Update asset balance
      (map-set asset-balances 
        {token: token-principal} 
        (+ (default-to u0 (map-get? asset-balances {token: token-principal})) amount))
      
      ;; Update user tier
      (unwrap-panic (update-user-tier tx-sender stx-equivalent))
      
      (print {event: "token-deposit", user: tx-sender, token: token-principal, amount: amount, shares: new-shares})
      (ok new-shares)
    )
  )
)

;; --- STX Withdraw (with dynamic fees) ---
(define-public (withdraw (user-shares uint))
  (begin
    (try! (when-not-paused))
    (try! (only-whitelisted))
    (let (
          (user-balance (default-to u0 (map-get? shares {user: tx-sender})))
          (vault-assets (var-get total-assets))
          (vault-shares (var-get total-shares))
      )
      (asserts! (<= user-shares user-balance) ERR-NO-SHARES)
      (let (
        (redeem-amount (/ (* user-shares vault-assets) vault-shares))
        (fee (get-user-withdraw-fee tx-sender redeem-amount))
        (to-user (- redeem-amount fee))
        (recipient (var-get fee-recipient))
      )
        (map-set shares {user: tx-sender} (- user-balance user-shares))
        (var-set total-shares (- vault-shares user-shares))
        (var-set total-assets (- vault-assets redeem-amount))
        
        ;; Handle fee transfer
        (try! (if (> fee u0)
          (as-contract (stx-transfer? fee tx-sender recipient))
          (ok true)))
        
        ;; Transfer remaining amount to user
        (try! (as-contract (stx-transfer? to-user tx-sender tx-sender)))
        
        (print {event: "withdraw", user: tx-sender, amount: to-user, shares: user-shares, fee: fee})
        (ok {amount: to-user, fee: fee})
      )
    )
  )
)

;; --- Emergency Withdraw (no fee, only in emergency mode) ---
(define-public (emergency-withdraw)
  (begin
    (asserts! (var-get emergency-mode) ERR-NOT-EMERGENCY)
    (let (
          (user-balance (default-to u0 (map-get? shares {user: tx-sender})))
          (vault-assets (var-get total-assets))
          (vault-shares (var-get total-shares))
      )
      (asserts! (> user-balance u0) ERR-NO-SHARES)
      (let (
            (redeem-amount (/ (* user-balance vault-assets) vault-shares))
          )
        (map-set shares {user: tx-sender} u0)
        (var-set total-shares (- vault-shares user-balance))
        (var-set total-assets (- vault-assets redeem-amount))
        (try! (as-contract (stx-transfer? redeem-amount tx-sender tx-sender)))
        (print {event: "emergency-withdraw", user: tx-sender, amount: redeem-amount, shares: user-balance, fee: u0})
        (ok {amount: redeem-amount, fee: u0})
      )
    )
  )
)

;; --- Strategy Interface (extendable) ---
(define-public (stake-into-strategy)
  (begin
    (try! (only-admin))
    (let ((assets (var-get total-assets)))
      (var-set staked-amount assets)
      (var-set total-assets u0)
      (ok true)
    )
  )
)

(define-public (harvest-yield)
  (begin
    (try! (only-admin))
    (let (
          (staked (var-get staked-amount))
          (yield (var-get fake-yield))
          (new-total (+ staked yield))
      )
      (var-set total-assets new-total)
      (var-set staked-amount u0)
      (var-set fake-yield u0)
      (print {event: "yield-harvested", amount: yield})
      (ok yield)
    )
  )
)

(define-public (simulate-yield (amount uint))
  (begin
    (try! (only-admin))
    (var-set fake-yield amount)
    (ok true)
  )
)

;; --- Rebalance (strategy switching) ---
(define-public (rebalance (to-strategy uint))
  (begin
    (try! (only-admin))
    (var-set strategy to-strategy)
    (ok true)
  )
)

;; --- Read-Only Helpers ---
(define-read-only (get-user-shares (user principal))
  (default-to u0 (map-get? shares {user: user}))
)

(define-read-only (get-user-tier-info (user principal))
  (let (
    (tier-data (default-to {tier: u0, total-volume: u0, last-deposit-height: u0} 
                           (map-get? user-tiers {user: user})))
    (current-tier (calculate-user-tier user))
  )
    (merge tier-data {calculated-tier: current-tier})
  )
)

(define-read-only (get-asset-config (token principal))
  (map-get? asset-configs {token: token})
)

(define-read-only (get-asset-balance (token principal))
  (default-to u0 (map-get? asset-balances {token: token}))
)

(define-read-only (get-total-assets) (var-get total-assets))
(define-read-only (get-total-shares) (var-get total-shares))
(define-read-only (get-strategy) (var-get strategy))

(define-read-only (get-share-price)
  (let ((assets (var-get total-assets))
        (total-shares-val (var-get total-shares)))
    (if (is-eq total-shares-val u0)
        u0
        (/ assets total-shares-val)
    )
  )
)

(define-read-only (get-fee-info)
  {
    base-withdraw-fee-bps: (var-get base-withdraw-fee-bps),
    performance-fee-bps: (var-get performance-fee-bps),
    fee-recipient: (var-get fee-recipient)
  }
)

(define-read-only (get-user-withdraw-fee-preview (user principal) (amount uint))
  (get-user-withdraw-fee user amount)
)

(define-read-only (is-paused) (var-get paused))
(define-read-only (is-emergency) (var-get emergency-mode))
(define-read-only (get-admin) (var-get admin))

(define-read-only (is-whitelisted (user principal))
  (default-to false (map-get? whitelist {user: user}))
)

;; Legacy compatibility
(define-public (set-withdraw-fee (fee-bps uint))
  (set-base-withdraw-fee fee-bps)
)