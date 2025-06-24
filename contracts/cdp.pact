; CDP for kUSD

(namespace (read-msg "ns"))

(module cdp 'cdp-admin-keyset
  @doc "CDP module: lock KDA collateral in individual vessels to borrow kUSD up to a maximum Loan-to-Value ratio; repay and redeem kUSD with time-based fee refunds and optional Stability Pool yield sharing; liquidate under-collateralized vaults using the Stability Pool’s kUSD backstop"

  ; -- Governance and Fee Pool Capabilities --
  (defcap GOVERNANCE ()
    @doc "Administrative capability enforced by cdp-admin-keyset"
    (enforce-keyset 'cdp-admin-keyset)
    (install-capability (FEE_POOL))
  )

  (defcap FEE_POOL ()
    @doc "Capability guarding the fee pool principal"
    true)

  (defun fee-pool-account:string ()
    @doc "Returns principal account for protocol fees"
    (create-principal (create-user-guard (require-capability (FEE_POOL)))))

  ; -- Configurable Parameters --
  (defschema configEntry
    @doc "Schema for configuration parameters"
    key:string
    value:decimal)
  (deftable config:{configEntry})

  (defun get-config:decimal (parameterName:string defaultValue:decimal)
    @doc "Retrieve config value or return default if not set"
    (if (contains parameterName (keys config))
        (at 'value (read config parameterName))
        defaultValue))

  (defun set-config:string (parameterName:string newValue:decimal)
    @doc "Governance: set protocol parameter"
    (with-capability (GOVERNANCE)
      (enforce (> newValue 0.0) "Parameter value must be positive")
      (update config parameterName { "value": newValue })
      "ConfigUpdated"))

  ; -- Constants --
  (defconst DEFAULT_MINIMUM_COLLATERAL_RATIO:decimal 110.0)
  (defconst DEFAULT_BORROW_FEE_PERCENTAGE:decimal 0.005)
  (defconst DEFAULT_FEE_REFUND_DAYS:decimal 182.0)
  (defconst DEFAULT_LIQUIDATION_REWARD_PERCENTAGE:decimal 0.005)
  (defconst DEFAULT_REDEMPTION_FEE_PERCENTAGE:decimal 0.005)

  ; -- Vessel Schema and Table --
  (defschema vessel
    @doc "Schema for vault state"
    owner:string
    vaultKey:string
    collateralAmount:decimal
    debtAmount:decimal
    lastBorrowTimestamp:time
    status:string)
  (deftable vessels:{vessel})

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
    true)

  (defcap REPAY_KUSD (owner:string amount:decimal)
    @doc "Capability to repay kUSD"
    @managed amount REPAY_MGR
    true)

  (defcap REDEEM_KUSD (redeemer:string amount:decimal)
    @doc "Capability to redeem kUSD"
    @managed amount REDEEM_MGR
    true)

  (defcap LIQUIDATE_VAULT (liquidator:string targetVault:string)
    @doc "Capability to liquidate a vault"
    true)

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

  (defun current-block-time:time ()
    @doc "Get current block time"
    (at 'block-time (chain-data)))


  ; jermaine suggested using capability guards, ( i dont know that concept )
  (defun enforce-reserve-vault:bool (vaultKey:string)
    @doc "Enforce vault principal access"
    (require-capability (RESERVE_VAULT vaultKey)))

  (defun generate-vault-key:string (ownerAccount:string)
    @doc "Generate unique vault key for owner"
    (format "vault-{}" [ownerAccount]))

  (defun get-vault-principal:string (vaultKey:string)
    @doc "Get principal for vault"
    (create-principal (create-user-guard (enforce-reserve-vault vaultKey))))

; mock oracle, should probably use price-feed, i didnt look at the on chain free.dai whatever contract. (don't even know if it is deployed)
; I also don't know what service updates to it? i know its dai or someothing, but where is it running?

  (defun fetch-kda-price:decimal ()
    @doc "Fetch valid KDA/USD price from oracle with freshness check"
      (with-read oracle-prices "KDA" { "price" := price, "timestamp" := timestamp }
        (let ((currentTime (current-block-time)))
          (enforce (> price 0.0) "Invalid oracle price")
          (enforce (< (- currentTime timestamp) 3600) "Oracle price stale")
          price)))

  (defun calculate-collateral-ratio:decimal (collateralAmount:decimal debtAmount:decimal)
    @doc "Calculate collateral ratio as percentage"
    (if (= debtAmount 0.0)
        9999.0
        (* (/ (* collateralAmount (fetch-kda-price)) debtAmount) 100.0)))

  ; -- Initialization --
  (defun init:object ()
    @doc "Initialize CDP tables and set protocol defaults"
    (with-capability (GOVERNANCE)
      (free.kusd-usd.create-account
        (fee-pool-account)
        (create-user-guard (require-capability (FEE_POOL))))
      (insert config "MIN_CR" { "value": DEFAULT_MINIMUM_COLLATERAL_RATIO })
      (insert config "BORROW_FEE_PCT" { "value": DEFAULT_BORROW_FEE_PERCENTAGE })
      (insert config "REFUND_DAYS" { "value": DEFAULT_FEE_REFUND_DAYS })
      (insert config "LIQUIDATION_REWARD" { "value": DEFAULT_LIQUIDATION_REWARD_PERCENTAGE })
      (insert config "REDEEM_FEE" { "value": DEFAULT_REDEMPTION_FEE_PERCENTAGE })
      { "status": "InitializationComplete" }))

  ; -- Vault Operations --
  (defun open-vault:string ()
    @doc "Create a new vessel with Inactive status"
    (let ((callerAccount (read-msg 'sender)))
        (enforce (not (contains callerAccount (keys vessels))) "Vault already exists")
        (let ((newVaultKey (generate-vault-key callerAccount)))
          (insert vessels callerAccount
            { "owner": callerAccount
            , "vaultKey": newVaultKey
            , "collateralAmount": 0.0
            , "debtAmount": 0.0
            , "lastBorrowTimestamp": (time "1970-01-01T00:00:00Z") ; curr-time
            , "status": "Inactive" })
          newVaultKey)))

  ; -- Collateral Operations --
  (defun deposit-collateral:string (depositAmount:decimal)
    @doc "Transfer KDA into an Active or Inactive vessel"
    (let ((callerAccount (read-msg 'sender)))
        (enforce (> depositAmount 0.0) "Deposit amount must be > 0")
        (with-read vessels callerAccount
          { "vaultKey" := vaultKey
          , "collateralAmount" := currentCollateralAmount
          , "status" := currentVesselStatus }
          (enforce (or (= currentVesselStatus "Inactive") (= currentVesselStatus "Active")) "Vessel is closed")
          ; i did transfer before, but this is more correct since it did not exist yet, 
          (coin.transfer-create callerAccount (get-vault-principal vaultKey) (create-user-guard (enforce-reserve-vault vaultKey)) depositAmount)
          (update vessels callerAccount { "collateralAmount": (+ currentCollateralAmount depositAmount) })
          "DepositCompleted")))

  (defun withdraw-collateral:string (withdrawAmount:decimal)
    @doc "Withdraw KDA from an Active vessel if CR holds"
    (let ((callerAccount (read-msg 'sender)))
        (enforce (> withdrawAmount 0.0) "Withdraw amount must be > 0")
        (with-read vessels callerAccount
          { "vaultKey" := vaultKey
          , "collateralAmount" := currentCollateralAmount
          , "debtAmount" := currentDebtAmount
          , "status" := currentVesselStatus }
          (enforce (= currentVesselStatus "Active") "Vessel must be Active")
          (let ((newCollateralAmount (- currentCollateralAmount withdrawAmount)))
            (enforce (>= (calculate-collateral-ratio newCollateralAmount currentDebtAmount)
                        (get-config "MIN_CR" DEFAULT_MINIMUM_COLLATERAL_RATIO))
                    "Collateral ratio too low")
            (coin.transfer (get-vault-principal vaultKey) callerAccount withdrawAmount)
            (update vessels callerAccount { "collateralAmount": newCollateralAmount })
            "WithdrawCompleted"))))

  (defun borrow-kusd:string (borrowAmount:decimal)
    @doc "Charge one-time fee, mint kUSD, update debt and status"
    (let ((callerAccount (read-msg 'sender)))
      (with-capability (BORROW_KUSD callerAccount borrowAmount) ; Some capability from the kusd-usd
        (with-read vessels callerAccount
          { "vaultKey" := vaultKey
          , "collateralAmount" := currentCollateralAmount
          , "debtAmount" := currentDebtAmount
          , "status" := currentVesselStatus }
          (enforce (or (= currentVesselStatus "Inactive") (= currentVesselStatus "Active")) "Vessel closed")
          (let* ((configuredBorrowFeePct (get-config "BORROW_FEE_PCT" DEFAULT_BORROW_FEE_PERCENTAGE))
                 (calculatedFeeAmount (* borrowAmount configuredBorrowFeePct))
                 (calculatedTotalDebtDelta (+ borrowAmount calculatedFeeAmount))
                 (newDebtTotal (+ currentDebtAmount calculatedTotalDebtDelta))
                 (newCollateralRatio (calculate-collateral-ratio currentCollateralAmount newDebtTotal)))
            (enforce (>= newCollateralRatio (get-config "MIN_CR" DEFAULT_MINIMUM_COLLATERAL_RATIO))
                    "Collateral ratio too low")

            ; since we don't want modrefs?

            ; Another idea would be, to totally ,forget about the mint and burn, we just pre-mint and store everything in a independent escow
            ; and make this contract owner of that escrow, and instead of mint and burn we do transfer. (this would be easier, and we could just escrow capabilities c: accounts)
            
            ; based on new input: if i understand emily's implementation of kusd correctly, 
            ; (defun add-contract-to-whitelist:string (contract:string whitelist-ref:module{whitelisted-module-iface}))
            ; it would mean the contract should conform the interface (whitelisted-module-iface) and we won't use capabilities?
            ; that contract can just call mint/burn from anywhere since we are owner of that contract? that  the call to burn above would work.
            ; but i donno i just uread the: https://www.notion.so/kadenateam/Kudos-Rewrite-Architecture-Initial-Draft-21be868b687880c48621d70caed4df70?showMoveTo=true&saveParent=true link
            ; That would mean we dont nee BORROW_MGR, REPAY_MGR, REDEEM_MGR, capabilities anymore.

            

            (free.kusd-usd.mint callerAccount borrowAmount) 
            (free.kusd-usd.mint (fee-pool-account) calculatedFeeAmount)
            (update vessels callerAccount
              { "debtAmount": newDebtTotal
              , "lastBorrowTimestamp": (current-block-time)
              , "status": "Active" })
            "BorrowCompleted")))))

; When a borrower calls repay-kusd, the contract:
;  1. Computes the original one-time borrow fee, the elapsed time since borrowing, and the prorated refundable amount.
;  2. Burns the net kUSD (repayment minus any refund) from the borrower’s wallet.
;  3. Transfers the refundable fee back to the borrower from the fee pool if applicable.
;  4. Updates the vault’s debtAmount and status (Inactive once fully repaid) and returns the appropriate completion message.

(defun repay-kusd:string (repayAmount:decimal)
  @doc "Burn kUSD net of refund, refund prorated fee, update debt and status"
  (let ((callerAccount (read-msg 'sender)))
    (with-capability (REPAY_KUSD callerAccount repayAmount) ; some cap fron the kusd-usd
      (with-read vessels callerAccount
        { "vaultKey"             := vaultKey
        , "debtAmount"           := currentDebtAmount
        , "lastBorrowTimestamp"  := lastBorrowTimestamp
        , "status"               := currentVesselStatus }
        (enforce (= currentVesselStatus "Active") "Vessel must be Active")


        (let ((configuredBorrowFeePct  (get-config "BORROW_FEE_PCT" DEFAULT_BORROW_FEE_PERCENTAGE))
               ; #Determine original borrow fee
               ; – backs out the 0.5% one-time fee from your total debt
               (basePrincipal          (/ currentDebtAmount (+ 1.0 configuredBorrowFeePct)))
               (originalFeeTotal       (- currentDebtAmount basePrincipal))


                ; # Compute time-based refund ratio
                ; receive a pro rata refund of the fixed borrowing fee… The smart contract manages this refund, ensuring transparency and accuracy.
               (elapsedSeconds         (diff-time (current-block-time) lastBorrowTimestamp))
               (elapsedDaysTotal       (/ elapsedSeconds 86400.0))
               (refundWindowDays       (get-config "REFUND_DAYS" DEFAULT_FEE_REFUND_DAYS))
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
          ; we cant just call this i know.
          (free.kusd-usd.burn callerAccount burnAmountNet) ; need a kusd-ref or something
          ; refund and update in one conditional branch
          ; Do we actually owe the borrower any fee‐refund?
          (if (> calculatedRefundAmt 0.0)
              (do
                ; install capability (since its sent from the contract and the contract signs it )
                (free.kusd-usd.transfer (fee-pool-account) callerAccount calculatedRefundAmt)
                (update vessels callerAccount
                  { "debtAmount": remainingDebtAfter
                  , "status":    updatedVesselStatus })
                "RepayCompleted")
              (do
                (update vessels callerAccount
                  { "debtAmount": remainingDebtAfter
                  , "status":    updatedVesselStatus })
                "NoRefund")))))))

; When a kUSD holder calls redeem-kusd, the contract:
;  1. Enforces the caller’s REDEEM_KUSD capability and minimum redeem amount.
;  2. Computes and collects the redemption fee into the fee-pool, then burns the remaining kUSD.
;  3. Verifies the fee-pool can cover 70% vault payouts, then gathers all vaults sorted by highest LTV.
;  4. Iteratively pays off each vault’s debt up to the holder’s net kUSD, transfers the matching KDA back to the redeemer, 
;     updates each vault’s collateral, debt, and status.
;  5. Distributes 70% of each vault’s share of the fee to that vault owner (leaving 30% in the pool) 
;     and ensures the entire kUSD amount was redeemed or reverts.

  (defun redeem-kusd:string (redeemAmount:decimal)
  @doc "Deduct fee, burn net kUSD, distribute fee, redeem KDA"
  (let ((callerAccount (read-msg 'sender)))
    (with-capability (REDEEM_KUSD callerAccount redeemAmount); some cap from the kusd-usd
      (enforce (>= redeemAmount 1.0) "Redemption amount must be >= 1 kUSD")

      (let* (
             ; Fetch current KDA price for valuation -spec: use oracle price
             (currentKdaPrice             (fetch-kda-price))
             ; Lookup redemption fee pct (e.g. 0.5%) - spec: fixed redemption fee
             (configuredRedemptionFeePct  (get-config "REDEEM_FEE" DEFAULT_REDEMPTION_FEE_PERCENTAGE))
             ; Compute total fee to collect -  spec: fee = redeemAmount * feePct
             (totalFeeCollected           (* redeemAmount configuredRedemptionFeePct))
             ; Net kUSD to burn after fee - spec: netRedeemable = redeemAmount - fee
             (netRedeemableAmount         (- redeemAmount totalFeeCollected))
             ; Vault-share of fee (70% of totalFeeCollected) - spec: 70% of fee to vaults
             (vaultsFeeShareTotal         (* totalFeeCollected 0.7)))

        ; we cant just call this i know
        (free.kusd-usd.burn callerAccount netRedeemableAmount)

        ; Transfer full fee into fee-pool account - collect full redemption fee 
        ; first add the 100% to the fee later, 70% will be distributed to vaults
        (free.kusd-usd.transfer callerAccount (fee-pool-account) totalFeeCollected)

        ; Ensure fee-pool has enough to cover vault shares - spec: enforce sufficient fee reserve
        (let ((feePoolBalance (free.kusd-usd.get-balance (fee-pool-account))))
          (enforce (>= feePoolBalance vaultsFeeShareTotal) "Insufficient fee reserve"))

        ; Gather all vault entries with computed LTV - spec: sort vessels by descending LTV
        ; this will take up way to much gas, any other way to do this?

        ; we get all vessels, and sort them by their LTV ratio
        (let* ((allVesselEntries
                 (map (lambda (vaultOwnerKey:string)
                        (with-read vessels vaultOwnerKey
                          { "collateralAmount" := vesselCollateral
                          , "debtAmount"       := vesselDebt
                          , "status"           := vesselStatus }
                          ;; compute each vault’s loan-to-value ratio
                          (let ((loanToValueRatio
                                 (if (or (= vesselCollateral 0.0) (= vesselDebt 0.0))
                                     0.0
                                     (* 100.0 (/ vesselDebt (* vesselCollateral currentKdaPrice))))))
                            { "vaultOwner":       vaultOwnerKey
                            , "collateralAmount": vesselCollateral
                            , "debtAmount":       vesselDebt
                            , "status":           vesselStatus
                            , "loanToValueRatio": loanToValueRatio })))
                      (keys vessels)))
               (sortedVesselEntries
                 (sort allVesselEntries
                       (lambda (firstEntry secondEntry)
                         (> (at 'loanToValueRatio firstEntry)
                            (at 'loanToValueRatio secondEntry)))))

               (initialAccumulatorMap
                 { "remainingRedeem":    netRedeemableAmount ; how much kUSD we still have to spend
                 , "totalKdaRedeemed":  0.0 }) ; how much KDA we redeemed so far starting at 0

               ;;Fold over sorted vaults, redeeming up to your kUSD
               (finalAccumulatorMap
                 (fold (lambda (accumulatorMap currentVesselEntry)
                         (let* ((remainingRedeemAmount (at 'remainingRedeem accumulatorMap))
                                (entryCollateral        (at 'collateralAmount   currentVesselEntry))
                                (entryDebt              (at 'debtAmount         currentVesselEntry))
                                (entryAvailableValue    (* entryCollateral currentKdaPrice))
                                ;; max kUSD value applied to this vault’s debt
                                (redeemValueForThisVault (min remainingRedeemAmount entryDebt entryAvailableValue))
                                ;; KDA to return to the user
                                (kdaToReturnToUser      (min entryCollateral (/ redeemValueForThisVault currentKdaPrice)))
                                ;; vault’s share of the fee
                                (vaultFeeShare          (* vaultsFeeShareTotal (/ redeemValueForThisVault redeemAmount)))
                                (updatedDebtRemaining   (max 0.0 (- entryDebt redeemValueForThisVault)))
                                 (updatedVesselStatus (cond
                                    ((= updatedDebtRemaining 0.0) "Redeemed")
                                    ((< updatedDebtRemaining entryDebt) "PartiallyRedeemed")
                                    (at "status" currentVesselEntry))))

                           ; never redeem more debt or collateral than available
                           (enforce (<= redeemValueForThisVault entryDebt) "Cannot redeem more than debt")
                           (enforce (<= kdaToReturnToUser entryCollateral) "Collateral overdraw")

                           (update vessels (at 'vaultOwner currentVesselEntry)
                             { "collateralAmount": (- entryCollateral kdaToReturnToUser)
                             , "debtAmount":       updatedDebtRemaining
                             , "status":           updatedVesselStatus })

                           ; Distribute vault’s fee share - spec: pay vaults 70% of fees
                           (free.kusd-usd.transfer (fee-pool-account)
                                                   (at 'vaultOwner currentVesselEntry)
                                                   vaultFeeShare)

                           ;Accumulate remaining and redeemed totals
                           { "remainingRedeem":   (- remainingRedeemAmount redeemValueForThisVault)
                           , "totalKdaRedeemed": (+ (at 'totalKdaRedeemed accumulatorMap) kdaToReturnToUser) }))
                       initialAccumulatorMap
                       sortedVesselEntries)))

          ; Ensure we used up all your kUSD - spec: enforce full redemption or fail
          (enforce (<= (at 'remainingRedeem finalAccumulatorMap) 0.0) "Not enough collateral to redeem")

          "RedeemCompleted")))))


      ;  To implement when using indexer
      ;
      ;  (defun redeem-kusd(vaultKey:string redeemAmount:decimal)
      ;    @doc "Redeem specific amount from a single vessel"
      ;    (let* (
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
      ;      (free.kusd-usd.burn 
      ;        (get-vault-principal vaultKey)
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

; When anyone calls liquidate-vault on an under-collateralized vessel, the contract:
;  1. Checks oracle freshness.
;  2. Computes the vault’s collateral ratio and requires it be below the protocol minimum.
;  3. Ensures the Stability Pool has enough kUSD to cover the vault’s outstanding debt.
;  4. Calculates the liquidator’s KDA reward and the remaining collateral to return to the pool.
;  5. Burns the vault’s kUSD debt from the Stability Pool, pays the liquidator their KDA bonus, sends the rest of the collateral into the pool, 
;     calls absorb-debt to absorb that collateral, and marks the vault as Liquidated.

(defun liquidate-vault:string (targetVaultKey:string)
  @doc "Liquidate undercollateralized vessel"
  (let ((liquidatorAccount (read-msg 'sender)))
    (with-capability (LIQUIDATE_VAULT liquidatorAccount targetVaultKey)
      (with-read vessels targetVaultKey
        { "collateralAmount" := vesselCollateralAmount
        , "debtAmount"       := vesselDebtAmount
        , "vaultKey"         := vaultKey }

        ; Check oracle freshness (<5 min)
        (with-read oracle-prices "KDA" { "timestamp" := oracleTimestamp }
          (enforce (< (- (current-block-time) oracleTimestamp) 300) ;  300 = 5 minutes
                   "Stale price for liquidation"))

        (let ((currentCollateralRatio
               (calculate-collateral-ratio vesselCollateralAmount vesselDebtAmount)))
          (enforce (< currentCollateralRatio
                      (get-config "MIN_CR" DEFAULT_MINIMUM_COLLATERAL_RATIO))
                   "Not eligible for liquidation")

          ; Stability Pool has enough kUSD to cover this debt
          (let* ((stabilityPoolUsdBalance
                  (free.kusd-usd.get-balance
                    (get-vault-principal (free.stability-pool.pool-key)))))
            (enforce (>= stabilityPoolUsdBalance vesselDebtAmount)
                     "Insufficient pool kUSD"))

          ; Calculate the liquidator’s reward in KDA and the remainder to the pool
          (let* ((calculatedRewardAmount
                   (* vesselCollateralAmount
                      (get-config "LIQUIDATION_REWARD" DEFAULT_LIQUIDATION_REWARD_PERCENTAGE)))
                 (collateralToReturnToPool
                   (- vesselCollateralAmount calculatedRewardAmount)))

            ; Burn the vault’s kUSD debt out of the Stability Pool
            ; Why do we do this: When the Stability Pool “burns” kUSD during a liquidation, 
            ; it permanently removes those tokens from circulation. With fewer kUSD in the system, 
            ; each remaining token becomes a bit more valuable. That shrinking supply helps keep kUSD trading close to its $1 target,
            ; because demand balances out the reduced supply.
            ; SO this is a crucial step!

            ;; we cant just call this i know
            (free.kusd-usd.burn
              (get-vault-principal (free.stability-pool.pool-key))
              vesselDebtAmount)


            ; Pay the liquidator their KDA reward
            (coin.transfer
              (get-vault-principal vaultKey)
              liquidatorAccount
              calculatedRewardAmount)

            ; Send the rest of the collateral into the Stability Pool
            (coin.transfer
              (get-vault-principal vaultKey)
              (get-vault-principal (free.stability-pool.pool-key))
              collateralToReturnToPool)

            ; Tell the Stability Pool to absorb that collateral as “debt paid”
            (free.stability-pool.absorb-debt collateralToReturnToPool)

            ; MNark it liquidated
            (update vessels targetVaultKey
              { "collateralAmount": 0.0
              , "debtAmount":       0.0
              , "status":           "Liquidated" })

            "LiquidationCompleted"))))))

; When anyone calls debt-ahead (a read-only view of what happens in the redeem-kusd), the contract:
;  1. Reads the current KDA/USD price from the oracle.
;  2. Loads the reference vault’s collateral and debt, and computes its Loan-to-Value ratio.
;  3. Iterates over all vaults, computing each vault’s LTV.
;  4. Sums the debt of every vault whose LTV exceeds the reference vault’s LTV.
;  5. Returns the total outstanding kUSD debt “ahead” of the specified vault in the redemption queue.

  (defun debt-ahead:decimal (referenceVaultKey:string)
    @doc "Returns total kUSD debt in vessels with higher LTV"
    (let* ((currentPrice (fetch-kda-price))
           (referenceEntry (read vessels referenceVaultKey))
           (refCollateral (at "collateralAmount" referenceEntry))
           (refDebt (at    "debtAmount" referenceEntry))
           (referenceLTV (if (or (= refCollateral 0.0) (= refDebt 0.0))
                            0.0
                            (* 100.0 (/ refDebt (* refCollateral currentPrice))))))
      (fold (lambda (accumulator:decimal vaultKey:string)
              (with-read vessels vaultKey
                { "collateralAmount" := candCollateral
                , "debtAmount" := candDebt }
                (let ((candLTV (if (or (= candCollateral 0.0) (= candDebt 0.0))
                                  0.0
                                  (* 100.0 (/ candDebt (* candCollateral currentPrice))))))
                  (if (> candLTV referenceLTV)
                      (+ accumulator candDebt)
                      accumulator))))
            0.0
            (keys vessels))))
)

;; -- Deployment --
(if (read-msg "upgrade")
  [ "upgraded contract" ]
  [ (create-table config)
    (create-table vessels)
    (create-table oracle-prices)
    (init) ])

; we still have the issue of debt-ahead a,d redeem-kusd, this functions can never execute , it will exhast gas so??
; pact gods how do we solve this?
