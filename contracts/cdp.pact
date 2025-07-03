; CDP for kUSD

(namespace (read-msg "ns"))

(module cdp GOVERNANCE
  @doc "CDP module: lock KDA collateral in individual vessels to borrow kUSD up to a maximum Loan-to-Value ratio; repay and redeem kUSD with time-based fee refunds and optional Stability Pool yield sharing; liquidate under-collateralized vaults using the Stability Pool’s kUSD backstop"
  
  ; -- Governance and Fee Pool Capabilities --
  (defcap GOVERNANCE ()
    @doc "Administrative capability enforced by cdp-admin-keyset"
    (enforce-keyset "cdp.cdp-admin-keyset")
  )

  (defcap FEE_POOL ()
    @doc "Capability guarding the fee pool principal"
    true)

  (defcap KUSD_MINT:bool () 
    @doc "Capability to mint kUSD"
    true
  )

  (defcap KUSD_BURN:bool () 
    @doc "Capability to mint kUSD"
    true
  )

  (defcap KDA_ABSORD:bool () 
    @doc "Capability to absord KDA from liquidated vessels into the stability pool"
    true
  )

  (defcap INTERNAL_COLLATERAL (account:string amount:decimal) 
    @doc "Internal capability to protect collateral update"
    true
  )

  (defcap INTERNAL_DEBT (account:string amount:decimal) 
    @doc "Internal capability to protect collateral update"
    true
  )


  (defcap DEPOSIT_COLLATERAL (account:string amount:decimal)
    @doc "Capability to withdraw collateral from a vessel"
    (enforce (> amount MIN_DEPOSIT) "Deposit amount must be bigger than minimum deposit")
    (with-read vessels account
      { "guard":= guard
      , "closed":= currentVesselClosed }
      (enforce-guard guard) ;; might skip for deposit ? 
      (enforce (not currentVesselClosed)  "Vessel is closed") ;; do we care when it's inactive? 
    ) 
    (compose-capability (INTERNAL_COLLATERAL account amount))
  )

  (defcap WITHDRAW_COLLATERAL (account:string amount:decimal)
    @doc "Capability to withdraw collateral from a vessel"
    (enforce (> amount MIN_WITHDRAWAL) "Withdraw amount must be > 0")
    (with-read vessels account
        { "vaultKey" := vaultKey
        , "guard":= guard
        , "collateralAmount" := currentCollateralAmount
        , "debtAmount" := currentDebtAmount
        , "closed" := closed }
        (enforce-guard guard)
        (enforce (not closed) "Vessel must not be closed")
        (enforce-min-collateral-ratio (- currentCollateralAmount amount) currentDebtAmount)
    )
    (compose-capability (INTERNAL_COLLATERAL account (- amount)))
    ;; signs for the vault guard
    (compose-capability (RESERVE_VAULT account))
  )

  (defun register-kda-absord-guard:string ()
    (cdp.stability-pool.register-kda-absord-guard (create-capability-guard (KDA_ABSORD)))
  )

  (defun register-kusd-mint-guard:string ()
    (cdp.kusd-usd.register-cdp-mint-guard (create-capability-guard (KUSD_MINT)))
  )
   
  (defun register-kusd-burn-guard:string ()
    (cdp.kusd-usd.register-cdp-burn-guard (create-capability-guard (KUSD_BURN)))
  )

  (defun fee-pool-account:string ()
    @doc "Returns principal account for protocol fees"
    (create-principal (create-capability-guard (FEE_POOL))))

  ; -- Configurable Parameters --
  (defschema configEntry
    @doc "Schema for configuration parameters"
    key:string
    value:decimal)

  (deftable config:{configEntry})

  (defun get-config:decimal (parameterName:string)
    @doc "Retrieve config value or return default if not set"
    (at 'value (read config parameterName)) )

  (defun set-config:string (parameterName:string newValue:decimal)
    @doc "Governance: set protocol parameter"
    (with-capability (GOVERNANCE)
      (enforce (> newValue 0.0) "Parameter value must be positive")
      (update config parameterName { "value": newValue })
      "ConfigUpdated"))

  ; -- Constants --
  (defconst MIN_DEPOSIT:decimal 0.0)
  (defconst MIN_WITHDRAWAL:decimal 0.0)
  
  (defconst DEFAULT_MINIMUM_COLLATERAL_RATIO:decimal 110.0)
  (defconst DEFAULT_BORROW_FEE_PERCENTAGE:decimal 0.005)
  (defconst DEFAULT_FEE_REFUND_DAYS:decimal 182.0)
  (defconst DEFAULT_LIQUIDATION_REWARD_PERCENTAGE:decimal 0.005)
  (defconst DEFAULT_REDEMPTION_FEE_PERCENTAGE:decimal 0.005)

  ; -- Vessel Schema and Table --
  (defschema vessel
    @doc "Schema for vault state"
    owner:string
    guard:guard
    vaultKey:string
    collateralAmount:decimal
    debtAmount:decimal
    lastBorrowTimestamp:time
    status:bool ;; active or inactive
    closed:bool ;; active/inactive/partially-redeemed or closed (liquidated/redeemed) )
  )
  (deftable vessels:{vessel})

  ;  (defconst PARTIALLY_REDEEMED:string "Partially-redeemed" ) 
  ;  (defconst STATUSES [PARTIALLY_REDEEMED REDEEMED LIQUIDATED])

  ; -- Events --

  (defcap COLLATERAL_UPDATED (vaultKey:string prevCollateralAmount:decimal newCollateralAmount:decimal)
    @doc "Event emitted when collateral is deposited into a vessel"
    @event true
  ) 

  (defcap DEBT_UPDATED (vaultKey:string prevDebtAmount:decimal newDebtAmount:decimal)
    @doc "Event emitted when collateral is deposited into a vessel"
    @event true
  ) 

  (defcap CONFIG_UPDATED (
        min-collateral-ratio:decimal
        borrow-fee-pct:decimal
        fee-refund-days:decimal
        liquidation-reward-percentage:decimal
        redemption-fee-percentage:decimal)
    @event
    true
  )

  ; -- Oracle Feed --
  (defschema priceFeed
    @doc "Schema for price feed data"
    symbol:string
    price:decimal
    timestamp:time)

  (deftable oracle-prices:{priceFeed})


  ; -- Operation Capabilities --
  (defcap RESERVE_VAULT (vaultKey:string)
    @doc "Capability guarding vault principal"
    true)

  (defcap BORROW_KUSD (owner:string amount:decimal)
    @doc "Capability to borrow kUSD"
    @managed amount BORROW_MGR
    (with-read vessels owner
      { "guard" := guard
      , "status" := currentVesselStatus }
      ;; why is "inactive vessel" allowed to borrow? 
      (enforce-guard guard)
      (enforce currentVesselStatus  "Vessel inactive or closed")
    )
  )

  (defcap REPAY_KUSD (account:string amount:decimal)
    @doc "Capability to repay kUSD"
    @managed amount REPAY_MGR
    (with-read vessels account
      { "guard" := guard
      , "status" := currentVesselStatus }
      (enforce-guard guard)
      (enforce currentVesselStatus  "Vessel inactive or closed")
      (compose-capability (INTERNAL_DEBT account amount))
    )
  )

  ;  (defun is-active (account:string)
  ;      {with-read vessels account
  ;        { "status" := currentVesselStatus }
  ;        (enforce currentVesselStatus "Vessel must be Active")
  ;      }
  ;      (> debt 0 )
  ;  ) 

  (defcap REDEEM_KUSD (redeemer:string amount:decimal)
    @doc "Capability to redeem kUSD"
    @managed amount REDEEM_MGR
    true)

  (defcap LIQUIDATE_VAULT (liquidator:string targetVault:string)
    @doc "Capability to liquidate a vault"
      (with-read vessels targetVault
        { "collateralAmount" := vesselCollateralAmount
        , "debtAmount"       := vesselDebtAmount
        , "vaultKey"         := vaultKey }

        ; Check oracle freshness (<5 min)
        (with-read oracle-prices "KDA" { "timestamp" := oracleTimestamp }
          (enforce (< (- (current-block-time) oracleTimestamp) 300) ;  300 = 5 minutes
                   "Stale price for liquidation"))

        (let (
          (min-coll-ratio:decimal (get-config "MIN_CR"))
          (currentCollateralRatio
               (calculate-collateral-ratio vesselCollateralAmount vesselDebtAmount)))
          (enforce (< currentCollateralRatio
                      min-coll-ratio)
                   "Not eligible for liquidation")

          ; Stability Pool has enough kUSD to cover this debt
          (let ((stabilityPoolUsdBalance
                  (cdp.kusd-usd.get-balance
                    (get-vault-principal (cdp.stability-pool.pool-key)))))
            (enforce (>= stabilityPoolUsdBalance vesselDebtAmount)
                     "Insufficient pool kUSD"))
  ) ))

  ; -- Managed Capability Functions --
  (defun BORROW_MGR:decimal (managed:decimal requested:decimal)
    @doc "Manages borrow amount to prevent replay"
    (enforce (>= managed requested) "Borrow amount exceeds limit")
    (- managed requested))

  (defun REPAY_MGR:decimal (managed:decimal requested:decimal)
    @doc "Manages repay amount to prevent replay"
    (enforce (>= managed requested) "Repay amount exceeds limit")
    (- managed requested))

  (defun REDEEM_MGR:decimal (managed:decimal requested:decimal)
    @doc "Manages redeem amount to prevent replay"
    (enforce (>= managed requested) "Redeem amount exceeds limit")
    (- managed requested))

  ; -- Helpers --
  (defun max:decimal (values:[decimal])
  "Compute the maximum of a list of decimals."
  (enforce (> (length values) 0) "List cannot be empty")
  (fold (lambda (x:decimal y:decimal) (if (>= x y) x y)) 
        (at 0 values) 
        (drop 1 values)))

  (defun min:decimal (values:[decimal])
  "Compute the minimum of a list of decimals"
  (enforce (> (length values) 0) "List cannot be empty")
  (fold (lambda (x:decimal y:decimal) (if (<= x y) x y)) 
        (at 0 values) 
        (drop 1 values)))


  (defun to-timestamp:integer (input:time)
    "Computes an Unix timestamp of the input date"
    (floor (diff-time input (time "1970-01-01T00:00:00Z")))
  )

  (defun current-block-time:time ()
    @doc "Get current block time"
    (at 'block-time (chain-data)))


  ;  ;; psuedo-code
  ;  (defun index-count ()
  ;    (with-read config 
  ;      {index:= idx }
  ;      index
  ;      )
  ;  )

  ;  (defun add-index () 
  ;    (update config "index" { "value": (+ (index-count) 1) }
  ;    (+ (index-count) 1)
  ;  )
  
  ;; 
  (defun generate-vault-key:string (ownerAccount:string)
    @doc "Generate unique vault key for owner"
    (format "vault-{}" [ownerAccount ]))
              
  (defun get-vault-guard:guard (account:string)
    (create-capability-guard (RESERVE_VAULT account)))

  (defun get-vault-principal:string (account:string)
    @doc "Get principal for vault"
    (create-principal (get-vault-guard account) )) 


; mock oracle, should probably use price-feed, i didnt look at the on chain free.dai whatever contract. (don't even know if it is deployed)
; I also don't know what service updates to it? i know its dai or someothing, but where is it running?

  (defun fetch-kda-price:decimal ()
    @doc "Fetch valid KDA/USD price from oracle with freshness check"
    (with-read oracle-prices "KDA" { "price" := price, "timestamp" := timestamp }
        (enforce (> price 0.0) "Invalid oracle price")
        (enforce (< (- (to-timestamp (current-block-time)) (to-timestamp timestamp)) 3600) "Oracle price stale")
        price))

  (defun calculate-collateral-ratio:decimal (collateralAmount:decimal debtAmount:decimal)
    @doc "Calculate collateral ratio as percentage"
    (if (= debtAmount 0.0)
        9999.0
        (* (/ (* collateralAmount (fetch-kda-price)) debtAmount) 100.0)))

  (defun enforce-min-collateral-ratio:bool (collateralAmount:decimal debtAmount:decimal)
    @doc "Enforce collateral ratio against minimum required"
    (let ( 
      (min-coll-ratio:decimal (get-config "MIN_CR"))
      (coll-ratio:decimal (calculate-collateral-ratio collateralAmount debtAmount)))
      (enforce (>= coll-ratio min-coll-ratio)
        "Collateral ratio too low"))
  )

  ; -- Initialization --
  (defun init:bool (min-collateral-ratio:decimal
                      borrow-fee-pct:decimal
                      fee-refund-days:decimal
                      liquidation-reward-percentage:decimal
                      redemption-fee-percentage:decimal)
    @doc "Initialize CDP tables and set protocol defaults"
    (with-capability (GOVERNANCE)
      (cdp.kusd-usd.create-account
        (fee-pool-account)
        (create-capability-guard (FEE_POOL)))
      (insert config "MIN_CR" { "key": "MIN_CR", "value": min-collateral-ratio  })
      (insert config "BORROW_FEE_PCT" { "key": "BORROW_FEE_PCT", "value": borrow-fee-pct })
      (insert config "REFUND_DAYS" { "key": "REFUND_DAYS", "value": fee-refund-days })
      (insert config "LIQUIDATION_REWARD" { "key": "LIQUIDATION_REWARD", "value": liquidation-reward-percentage })
      (insert config "REDEEM_FEE" { "key": "REDEEM_FEE",  "value": redemption-fee-percentage })
      )
      (emit-event (CONFIG_UPDATED 
        min-collateral-ratio 
        borrow-fee-pct 
        fee-refund-days 
        liquidation-reward-percentage 
        redemption-fee-percentage)
      )
  )

  ; -- Utility --
  
  (defun update-collateral-amount:string (account:string newCollateralAmount:decimal)
    @doc "Update collateral amount without transferring KDA"
    (require-capability (INTERNAL_COLLATERAL account newCollateralAmount))
    (with-read vessels account 
      { "vaultKey" := vaultKey,
        "collateralAmount" := currentCollateralAmount } 
      (update vessels account { "collateralAmount": (+ currentCollateralAmount newCollateralAmount) })
      (emit-event (COLLATERAL_UPDATED vaultKey currentCollateralAmount (+ currentCollateralAmount newCollateralAmount)) )
    )
    "collateral updated"
  )

  ;Why needed: Aligns incentives with market conditions
    ; During normal dips: Lower rewards save protocol funds
    ; During crashes: Higher rewards ensure rapid liquidations
    ; Base minimum: Guarantees liquidation viability
  (defun liquidation-reward:decimal (currentCR:decimal minCR:decimal)
    @doc "Calculate dynamic liquidation reward based on risk severity"
    ; Formula: (minCR - currentCR) / minCR
    ; Example: minCR=110%, currentCR=108% -> (110-108)/110 = 1.82%
    (let ((severity (max 0.0 (/ (- minCR currentCR) minCR)))) ; Severity range: [0, 1]
    
    ;Final reward percentage
      ; Base: 0.5% minimum reward
      ;Risk premium: Additional 0-1.5% scaled by severity
      ; Formula: 0.005 + (severity × 0.015)
    (+ 0.005 (* severity 0.015)))) ; 0.005 + (1.0 × 0.015) = 0.02 = 2.0%
  
  ;; borrow only
  (defun update-debt-amount (account:string newDebtAmount:decimal)
    @doc "Update debt amount without transferring KDA"
    (require-capability (INTERNAL_DEBT account newDebtAmount))
    ;; TODO: if positive add timestamp, if neg not 
    (with-read vessels account { 
      "vaultKey" := vaultKey,
      "debtAmount" := currDebtAmount } 
      (update vessels account { 
      , "debtAmount": (+ currDebtAmount newDebtAmount)
      , "lastBorrowTimestamp": (current-block-time) })
      (emit-event (DEBT_UPDATED vaultKey currDebtAmount (+ currDebtAmount newDebtAmount)) ))
  )

  ; -- Collateral Operations --

  (defun open-vault:string (account:string guard:guard initialDeposit:decimal)
    @doc "Create a new vessel and deposit initial collateral" 
    (let ((vaultKey:string (generate-vault-key account)) )
      (insert vessels account
        { "owner": account
        ;  , "vaultIndex" :  (add-index)
        , "guard": guard ;; user-guards u: account
        , "vaultKey": vaultKey
        , "collateralAmount": initialDeposit
        , "debtAmount": 0.0
        , "lastBorrowTimestamp": (time "1970-01-01T00:00:00Z")
        , "status": true
        , "closed": false })
        (with-capability (DEPOSIT_COLLATERAL account initialDeposit)
        ;; TODO: duplicate table update
          (coin.transfer-create account (get-vault-principal account) (get-vault-guard account) initialDeposit)
          (update-collateral-amount account initialDeposit)
        )
  ))

  (defun deposit-collateral:string (account:string depositAmount:decimal)
    @doc "Transfer KDA into an Active or Inactive vessel"
      (with-read vessels account {
        "vaultKey" := vaultKey
      }
      (coin.transfer-create account (get-vault-principal account) (get-vault-guard account) depositAmount)
      (with-capability (DEPOSIT_COLLATERAL account depositAmount)
        (update-collateral-amount account depositAmount)
      )
    )
  )

  (defun withdraw-collateral:string (account:string withdrawAmount:decimal)
    @doc "Withdraw KDA from an Active vessel if CR holds"
    (with-capability (WITHDRAW_COLLATERAL account withdrawAmount)
      (install-capability (coin.TRANSFER (get-vault-principal account) account withdrawAmount))
      (coin.transfer (get-vault-principal account) account withdrawAmount)
      (update-collateral-amount account (- withdrawAmount))
    )
  )

  (defun borrow-kusd:string (account:string borrowAmount:decimal)
    @doc "Charge one-time fee, mint kUSD, update debt and status"
      (with-capability (BORROW_KUSD account borrowAmount)
        (with-read vessels account
          { "vaultKey" := vaultKey
          , "collateralAmount" := currentCollateralAmount
          , "debtAmount" := currentDebtAmount
          }
          ;; update 
          (let ((configuredBorrowFeePct (get-config "BORROW_FEE_PCT"))
                (calculatedFeeAmount (* borrowAmount configuredBorrowFeePct))
                (calculatedTotalDebtDelta (+ borrowAmount calculatedFeeAmount))
                (newDebtTotal (+ currentDebtAmount calculatedTotalDebtDelta))
            )
            (enforce-min-collateral-ratio currentCollateralAmount newDebtTotal)
            (with-capability (KUSD_MINT account borrowAmount)
              (cdp.kusd-usd.mint account borrowAmount) 
              (cdp.kusd-usd.mint (fee-pool-account) calculatedFeeAmount)
            )
          (with-capability (INTERNAL_DEBT)
            (update-debt-amount account newDebtTotal)
          )
  ))))

; When a borrower calls repay-kusd, the contract:
;  1. Computes the original one-time borrow fee, the elapsed time since borrowing, and the prorated refundable amount.
;  2. Burns the net kUSD (repayment minus any refund) from the borrower’s wallet.
;  3. Transfers the refundable fee back to the borrower from the fee pool if applicable.
;  4. Updates the vault’s debtAmount and status (Inactive once fully repaid) and returns the appropriate completion message.

;; helper function to calculate calculatedRefundAmt

(defun repay-kusd:string (account:string repayAmount:decimal)
  @doc "Burn kUSD net of refund, refund prorated fee, update debt and status"
    (with-capability (REPAY_KUSD account repayAmount) ; some cap fron the kusd-usd
      (with-read vessels account
        { "vaultKey"             := vaultKey
        , "debtAmount"           := currentDebtAmount
        , "lastBorrowTimestamp"  := lastBorrowTimestamp
        }

        (let ((configuredBorrowFeePct  (get-config "BORROW_FEE_PCT"))
               ; #Determine original borrow fee
               ; – backs out the 0.5% one-time fee from your total debt
               (basePrincipal          (/ currentDebtAmount (+ 1.0 configuredBorrowFeePct)))
               (originalFeeTotal       (- currentDebtAmount basePrincipal))


                ; # Compute time-based refund ratio
                ; receive a pro rata refund of the fixed borrowing fee… The smart contract manages this refund, ensuring transparency and accuracy.
               (elapsedSeconds         (diff-time (current-block-time) lastBorrowTimestamp))
               (elapsedDaysTotal       (/ elapsedSeconds 86400.0))
               (refundWindowDays       (get-config "REFUND_DAYS"))
               (timeBasedRefundRatio   (max 0.0 (/ (- refundWindowDays elapsedDaysTotal) refundWindowDays)))
               (maxRefundableFee       (* originalFeeTotal timeBasedRefundRatio))

               ; # Prorate for partial repayment
               ; scales the refund by how much of your debt you actually repay
               (repaymentFraction      (min 1.0 (/ repayAmount currentDebtAmount)))
               (calculatedRefundAmt    (* maxRefundableFee repaymentFraction))

               ; # Burn net kUSD, refund fee
               ; you burn (destroy) exactly your repayment minus refund, and then the contract transfers your refundable fee back to you
               (burnAmountNet          (- repayAmount calculatedRefundAmt))
               (remainingDebtAfter     (max 0.0 (- currentDebtAmount burnAmountNet)))
               (updatedVesselStatus    (if (= remainingDebtAfter 0.0) "Inactive" "Active")))
          ; burn net amount
          (with-capability (KUSD_BURN account burnAmountNet) ; some cap from the kusd-usd
            (cdp.kusd-usd.burn account burnAmountNet) 
          )
          ; refund and update in one conditional branch
          ; Do we actually owe the borrower any fee‐refund?
          (if (> calculatedRefundAmt 0.0)
              (do
                ; install capability (since its sent from the contract and the contract signs it )
                (cdp.kusd-usd.transfer (fee-pool-account) account calculatedRefundAmt)
                (update vessels account
                  { "debtAmount": remainingDebtAfter
                  , "status":    updatedVesselStatus }))
                (update vessels account
                  { "debtAmount": remainingDebtAfter
                  , "status":    updatedVesselStatus })
                )))
        )
)

; When anyone calls liquidate-vault on an under-collateralized vessel, the contract:
;  1. Checks oracle freshness.
;  2. Computes the vault’s collateral ratio and requires it be below the protocol minimum.
;  3. Ensures the Stability Pool has enough kUSD to cover the vault’s outstanding debt.
;  4. Calculates the liquidator’s dynamic KDA reward and the remaining collateral to return to the pool.
;  5. Burns the vault’s kUSD debt from the Stability Pool, pays the liquidator their KDA bonus, sends the rest of the collateral into the pool, 
;     calls absorb-debt to absorb that collateral, and marks the vault as Liquidated.
(defun liquidate-vault:string (liquidatorAccount:string targetVaultKey:string)
  @doc "Liquidate undercollateralized vessel using dynamic risk-based rewards"
  (with-capability (LIQUIDATE_VAULT liquidatorAccount targetVaultKey)
    (with-read vessels targetVaultKey
      { "collateralAmount" := vesselCollateralAmount
      , "debtAmount"       := vesselDebtAmount
      , "vaultKey"         := vaultKey }


    ; Burn the vault’s kUSD debt out of the Stability Pool
    ; Why do we do this: When the Stability Pool “burns” kUSD during a liquidation, 
    ; it permanently removes those tokens from circulation. With fewer kUSD in the system, 
    ; each remaining token becomes a bit more valuable. That shrinking supply helps keep kUSD trading close to its $1 target,
    ; because demand balances out the reduced supply.
    ; SO this is a crucial step!

          
      ; Added extra check ensuring vault is eligable for liquidation by computing current CR & protocol minimum 
      ; and enforcing the under-collateralization
      (let ((currentCollateralRatio (calculate-collateral-ratio vesselCollateralAmount vesselDebtAmount))
            (minimumCollateralRatio     (get-config "MIN_CR" DEFAULT_MINIMUM_COLLATERAL_RATIO)))
        (enforce (< currentCollateralRatio minimumCollateralRatio) "Vault not eligible for liquidation")

        ; Calculate the dynamic KDA reward ( instead of the 0.5, we calculate a dynamic reward ratio )
        ; this in order to keep insentives interesting for liquidators
        ; https://medium.com/@whiterabbit_hq/black-thursday-for-makerdao-8-32-million-was-liquidated-for-0-dai-36b83cac56b6
        ; So on kadena even without gas spikes or zero‐bid auctions (makerDao), 
        ; liquidators need strong incentives to step in during extreme price moves.  
        ; A flat 0.5% reward may leave vaults uncleared when KDA crashes 60% in minutes, risking under‐collateralized debt.  
        ; Dynamic, severity-scaling rewards align keeper profit with protocol health, ensuring liquidations always happen.  
        ; Our Stability Pool backstop handles debt absorption, but liquidator participation is still critical to trigger absorb‐debt.  
        ; Severity-adjusted fees guarantee timely vault clearing and protect kUSD’s $1 peg—even when markets move violently.  

        (let ((rewardPercentage (liquidation-reward currentCollateralRatio minimumCollateralRatio))
              (collateralToPool  (- vesselCollateralAmount
                (* vesselCollateralAmount rewardPct))))
          
          ; Burn the vault’s kUSD debt
          (cdp.kusd-usd.burn
            (get-vault-principal (cdp.stability-pool.pool-key))
            vesselDebtAmount)

          ; Pay liquidator risk-adjusted reward
          (coin.transfer
            (get-vault-principal vaultKey)
            liquidatorAccount
            (* vesselCollateralAmount rewardPercentage))

          ; Return remaining collateral to pool
          (coin.transfer
            (get-vault-principal vaultKey)
            (get-vault-principal (cdp.stability-pool.pool-key))
            collateralToPool)

          ; Absord to stability pool
          (cdp.stability-pool.absorb-debt collateralToPool)

          ; Liquidated!
          (update vessels targetVaultKey
            { "collateralAmount": 0.0
            , "debtAmount":       0.0
            , "status":           "Liquidated" })

          "LiquidationCompleted")))))

;  When a kUSD holder calls redeem-kusd, the contract:
;  1. Enforces the caller’s REDEEM_KUSD capability and minimum redeem amount.
;  2. Computes and collects the redemption fee into the fee-pool, then burns the remaining kUSD.
;  3. Verifies the fee-pool can cover 70% vault payouts, then gathers all vaults sorted by highest LTV.
;  4. Iteratively pays off each vault’s debt up to the holder’s net kUSD, transfers the matching KDA back to the redeemer, 
;     updates each vault’s collateral, debt, and status.
;  5. Distributes 70% of each vault’s share of the fee to that vault owner (leaving 30% in the pool) 
;     and ensures the entire kUSD amount was redeemed or reverts.

;    (defun redeem-kusd:string (account:string redeemAmount:decimal)
;      @doc "Deduct fee, burn net kUSD, distribute fee, redeem KDA"
;        (with-capability (REDEEM_KUSD account redeemAmount); some cap from the kusd-usd
;          (enforce (>= redeemAmount 1.0) "Redemption amount must be >= 1 kUSD")

;          (let (
;               ; Fetch current KDA price for valuation -spec: use oracle price
;               (currentKdaPrice             (fetch-kda-price))
;               ; Lookup redemption fee pct (e.g. 0.5%) - spec: fixed redemption fee
;               (configuredRedemptionFeePct  (get-config "REDEEM_FEE" DEFAULT_REDEMPTION_FEE_PERCENTAGE))
;               ; Compute total fee to collect -  spec: fee = redeemAmount * feePct
;               (totalFeeCollected           (* redeemAmount configuredRedemptionFeePct))
;               ; Net kUSD to burn after fee - spec: netRedeemable = redeemAmount - fee
;               (netRedeemableAmount         (- redeemAmount totalFeeCollected))
;               ; Vault-share of fee (70% of totalFeeCollected) - spec: 70% of fee to vaults
;               (vaultsFeeShareTotal         (* totalFeeCollected 0.7)))

;            ; we cant just call this i know
;            (cdp.kusd-usd.burn account netRedeemableAmount)

;            ; Transfer full fee into fee-pool account - collect full redemption fee 
;            ; first add the 100% to the fee later, 70% will be distributed to vaults
;            (cdp.kusd-usd.transfer account (fee-pool-account) totalFeeCollected)

;            ; Ensure fee-pool has enough to cover vault shares - spec: enforce sufficient fee reserve
;            (let ((feePoolBalance (cdp.kusd-usd.get-balance (fee-pool-account))))
;              (enforce (>= feePoolBalance vaultsFeeShareTotal) "Insufficient fee reserve"))

;            ; Gather all vault entries with computed LTV - spec: sort vessels by descending LTV
;            ; this will take up way to much gas, any other way to do this?

;            ; we get all vessels, and sort them by their LTV ratio
;            (let ((allVesselEntries
;                    (map (lambda (vaultOwnerKey:string)
;                            (with-read vessels vaultOwnerKey
;                              { "collateralAmount" := vesselCollateral
;                              , "debtAmount"       := vesselDebt
;                              , "status"           := vesselStatus }
;                              ;; compute each vault’s loan-to-value ratio
;                              (let ((loanToValueRatio
;                                    (if (or (= vesselCollateral 0.0) (= vesselDebt 0.0))
;                                        0.0
;                                        (* 100.0 (/ vesselDebt (* vesselCollateral currentKdaPrice))))))
;                                { "vaultOwner":       vaultOwnerKey
;                                , "collateralAmount": vesselCollateral
;                                , "debtAmount":       vesselDebt
;                                , "status":           vesselStatus
;                                , "loanToValueRatio": loanToValueRatio })))
;                          (keys vessels)))
;                  (sortedVesselEntries
;                    (sort allVesselEntries
;                          (lambda (firstEntry secondEntry)
;                            (> (at 'loanToValueRatio firstEntry)
;                                (at 'loanToValueRatio secondEntry)))))

;                  (initialAccumulatorMap
;                    { "remainingRedeem":    netRedeemableAmount ; how much kUSD we still have to spend
;                    , "totalKdaRedeemed":  0.0 }) ; how much KDA we redeemed so far starting at 0

;                  ;;Fold over sorted vaults, redeeming up to your kUSD
;                  (finalAccumulatorMap
;                    (fold (lambda (accumulatorMap currentVesselEntry)
;                            (let ((remainingRedeemAmount (at 'remainingRedeem accumulatorMap))
;                                    (entryCollateral        (at 'collateralAmount   currentVesselEntry))
;                                    (entryDebt              (at 'debtAmount         currentVesselEntry))
;                                    (entryAvailableValue    (* entryCollateral currentKdaPrice))
;                                    ;; max kUSD value applied to this vault’s debt
;                                    (redeemValueForThisVault (min remainingRedeemAmount entryDebt entryAvailableValue))
;                                    ;; KDA to return to the user
;                                    (kdaToReturnToUser      (min entryCollateral (/ redeemValueForThisVault currentKdaPrice)))
;                                    ;; vault’s share of the fee
;                                    (vaultFeeShare          (* vaultsFeeShareTotal (/ redeemValueForThisVault redeemAmount)))
;                                    (updatedDebtRemaining   (max 0.0 (- entryDebt redeemValueForThisVault)))
;                                    (updatedVesselStatus (cond
;                                        ((= updatedDebtRemaining 0.0) "Redeemed")
;                                        ((< updatedDebtRemaining entryDebt) "PartiallyRedeemed")
;                                        (at "status" currentVesselEntry))))

;                              ; never redeem more debt or collateral than available
;                              (enforce (<= redeemValueForThisVault entryDebt) "Cannot redeem more than debt")
;                              (enforce (<= kdaToReturnToUser entryCollateral) "Collateral overdraw")

;                              (update vessels (at 'vaultOwner currentVesselEntry)
;                                { "collateralAmount": (- entryCollateral kdaToReturnToUser)
;                                , "debtAmount":       updatedDebtRemaining
;                                , "status":           updatedVesselStatus })

;                              ; Distribute vault’s fee share - spec: pay vaults 70% of fees
;                              (cdp.kusd-usd.transfer (fee-pool-account)
;                                                      (at 'vaultOwner currentVesselEntry)
;                                                      vaultFeeShare)

;                              ;Accumulate remaining and redeemed totals
;                              { "remainingRedeem":   (- remainingRedeemAmount redeemValueForThisVault)
;                              , "totalKdaRedeemed": (+ (at 'totalKdaRedeemed accumulatorMap) kdaToReturnToUser) }))
;                          initialAccumulatorMap
;                          sortedVesselEntries)))

;              ; Ensure we used up all your kUSD - spec: enforce full redemption or fail
;              (enforce (<= (at 'remainingRedeem finalAccumulatorMap) 0.0) "Not enough collateral to redeem")

;              "RedeemCompleted"))))


      ;  To implement when using indexer
      ;
      ;  (defun redeem-kusd(vaultKey:string redeemAmount:decimal)
      ;    @doc "Redeem specific amount from a single vessel"
      ;    (let (
      ;          ; Fetch the up-to-date KDA/USD price from the oracle
      ;          (currentPrice (fetch-kda-price))

      ;          ; Load the vault record by its key
      ;          (vessel (read vessels vaultKey))

      ;          ;Compute the maximum kUSD you can redeem from this vault:
      ;          ; You cannot exceed the vault’s outstanding debt
      ;          ; You cannot extract more value than its collateral is worth
      ;          (maxRedeemable
      ;            (min
      ;              ; outstanding debt in kUSD
      ;              (at 'debtAmount vessel)
      ;              ; collateral value in kUSD = collateralAmount * price
      ;              (* (at 'collateralAmount vessel) currentPrice)))

      ;          ;Determine how much to actually redeem:
      ;          ; the lesser of what the user asked and the vault’s maxRedeemable
      ;          (amountToRedeem (min redeemAmount maxRedeemable))

      ;          ; Convert that kUSD amount into KDA to send back
      ;          (kdaToSend (/ amountToRedeem currentPrice)))

      ;      (enforce (> amountToRedeem 0)
      ;              "Nothing to redeem from this vessel")

      ;      ;Update the vault’s on-chain state:
      ;      ; - Subtract the redeemed KDA from its collateral
      ;      ; - Subtract the redeemed kUSD from its debt
      ;      (update vessels vaultKey {
      ;        "collateralAmount": (- (at 'collateralAmount vessel) kdaToSend)
      ;        , "debtAmount":       (- (at 'debtAmount vessel) amountToRedeem)
      ;      })

      ;      ;Burn the kUSD from the vault’s kUSD balance / (this wont work) <-- see code we need the pact gods
      ;      (cdp.kusd-usd.burn 
      ;        (get-vault-principal account)
      ;        amountToRedeem)

      ;      ;Transfer the corresponding KDA back to the user
      ;      (coin.transfer
      ;        (fee-pool-account)
      ;        (read-msg 'sender)
      ;        kdaToSend)

      ;      ; Improrant: We emit the event so off-chain indexers/frontends see the new vault state
      ;      (emit-vessel-event
      ;        vaultKey
      ;        (at 'owner vessel)
      ;        ; new collateral left
      ;        (- (at 'collateralAmount vessel) kdaToSend)
      ;        ; new debt left
      ;        (- (at 'debtAmount vessel) amountToRedeem)
      ;        ; status = "Redeemed" if fully cleared, else remain "Active"
      ;        (if (= (- (at 'debtAmount vessel) amountToRedeem) 0.0)
      ;            "Redeemed" "Active"))

      ;      (format "Redeemed {} kUSD from vessel {}"
      ;              [amountToRedeem vaultKey])
      ;  ))

; When anyone calls debt-ahead (a read-only view of what happens in the redeem-kusd), the contract:
;  1. Reads the current KDA/USD price from the oracle.
;  2. Loads the reference vault’s collateral and debt, and computes its Loan-to-Value ratio.
;  3. Iterates over all vaults, computing each vault’s LTV.
;  4. Sums the debt of every vault whose LTV exceeds the reference vault’s LTV.
;  5. Returns the total outstanding kUSD debt “ahead” of the specified vault in the redemption queue.

  ;  (defun debt-ahead:decimal (referenceVaultKey:string)
  ;    @doc "Returns total kUSD debt in vessels with higher LTV"
  ;    (let ((currentPrice (fetch-kda-price))
  ;           (referenceEntry (read vessels referenceVaultKey))
  ;           (refCollateral (at "collateralAmount" referenceEntry))
  ;           (refDebt (at    "debtAmount" referenceEntry))
  ;           (referenceLTV (if (or (= refCollateral 0.0) (= refDebt 0.0))
  ;                            0.0
  ;                            (* 100.0 (/ refDebt (* refCollateral currentPrice))))))
  ;      (fold (lambda (accumulator:decimal vaultKey:string)
  ;              (with-read vessels vaultKey
  ;                { "collateralAmount" := candCollateral
  ;                , "debtAmount" := candDebt }
  ;                (let ((candLTV (if (or (= candCollateral 0.0) (= candDebt 0.0))
  ;                                  0.0
  ;                                  (* 100.0 (/ candDebt (* candCollateral currentPrice))))))
  ;                  (if (> candLTV referenceLTV)
  ;                      (+ accumulator candDebt)
  ;                      accumulator))))
  ;            0.0
  ;            (keys vessels))))
)

;; -- Deployment --
(if (read-msg "upgrade")
  [ "upgraded contract" ]
  [ (create-table config)
    (create-table vessels)
    (create-table oracle-prices)
    (init 
      DEFAULT_MINIMUM_COLLATERAL_RATIO 
      DEFAULT_BORROW_FEE_PERCENTAGE 
      DEFAULT_FEE_REFUND_DAYS 
      DEFAULT_LIQUIDATION_REWARD_PERCENTAGE 
      DEFAULT_REDEMPTION_FEE_PERCENTAGE ) ])

; we still have the issue of debt-ahead a,d redeem-kusd, this functions can never execute , it will exhast gas so??
; pact gods how do we solve this?

