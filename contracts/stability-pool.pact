; Stability Pool Contract

(namespace (read-msg "ns"))

(module stability-pool 'pool-admin-keyset
  @doc "Stability Pool: hold kUSD deposits to absorb vault liquidations by exchanging pooled kUSD for KDA collateral; depositors earn KDA yield proportional to their pool share and can withdraw or redeem without impacting core CDP operations"


  ;  -- Capabilities -- 
  
  (defcap GOVERNANCE () 
    (enforce-keyset 'pool-admin-keyset))
  (defcap POOL-RESERVE (poolKey:string) true)
  (defcap READ-ORACLE () 
    (enforce-keyset 'oracle-keyset))
  (defcap DEPOSIT-KUSD () @event true)
  (defcap ABSORB-DEBT (kdaAmount:decimal) @event true)
  (defcap WITHDRAW () @event true)
  (defcap REDEEM-KUSD (amount:decimal) @event true)

  (defcap WITHDRAW_EXECUTED (user:string amount:decimal kda:decimal)
    @event
    true
  )

  ; -- Schemas & Tables -- 

  ; cumulativeGain
  ; The pool keeps a running total of KDA-per-kUSD it has absorbed over time. 
  ; Every time a liquidation happens, it calculates how much KDA each deposited kUSD should earn and adds that increment to cumulativeGain.

  (defschema state
    totalDeposits:decimal
    cumulativeGain:decimal)
  (deftable pool-state:{state})

  (defschema claim
    deposit:decimal
    gainSnapshot:decimal)
  (deftable claims:{claim})

  (defconst REDEEM-FEE-PCT 0.005)


  ; --  Pool Helpers --
  (defun pool-key:string ()
    "Returns the identifier key for the main stability pool"
    "main")

  (defun enforce-reserve:bool
    (key:string)
    (require-capability (POOL-RESERVE key)))

  (defun create-pool-guard:guard
    (key:string)
    (create-user-guard (enforce-reserve key)))

  (defun pool-principal (key:string)
    (create-principal (create-pool-guard key)))

  ; -- Reads --

  (defun get-pool-state (poolKey:string)
    @doc "Return an object with totalDeposits and cumulativeGain from the pool"
    (with-read pool-state poolKey
      { "totalDeposits":= totalDeposits
      , "cumulativeGain":= cumulativeGain }
      { "totalDeposits": totalDeposits
      , "cumulativeGain": cumulativeGain }))

  (defun get-claim (claimKey:string)
    @doc "Return an object with deposit and gainSnapshot for a given claim entry"
    (with-read claims claimKey
      { "deposit":= deposit
      , "gainSnapshot":= gainSnapshot }
      { "deposit":       deposit
      , "gainSnapshot":  gainSnapshot }))

  ; ----------------------------------------------------------------------
  ; Initialization
  ; ----------------------------------------------------------------------
  (defun init-pool:string ()
    @doc "Governance: initialize the stability pool state and create its account"
    (with-capability (GOVERNANCE)
      (with-default-read pool-state (pool-key)
        { "totalDeposits": -1.0, "cumulativeGain": 0.0 }
        { "totalDeposits":= td, "cumulativeGain":= cg }
        (enforce (= td -1.0) "Pool already initialized"))
      (insert pool-state (pool-key)
        { "totalDeposits": 0.0
        , "cumulativeGain": 0.0 })
      (let ((pool-principal (pool-principal (pool-key)))
            (pool-guard (create-pool-guard (pool-key))))
        (free.kusd-usd.create-account pool-principal pool-guard)) ; create the initial pool account
      "Pool initialized"))


  ; When a depositor adds kUSD to the Stability Pool, the contract:
  ; 1. Checks the deposit amount is positive.
  ; 2. Transfers the kUSD from the user’s wallet into the pool account.
  ; 3. Updates the pool’s totalDeposits and takes a snapshot of the pool’s cumulative gain.
  ; 4. Inserts or updates the depositor’s claim entry with their new deposit and the pool snapshot, 
  ;    so they’ll earn KDA yield proportionally on future liquidations.
  (defun deposit-kusd (depositAmount:decimal)
    @doc "Deposit kUSD into the stability pool"
    (let* ((depositorAccount (read-msg 'sender))
           (poolAccount      (pool-principal (pool-key))))
      (enforce (> depositAmount 0.0) "Deposit amount must be > 0")
      (with-capability (DEPOSIT-KUSD)
        (with-capability (POOL-RESERVE (pool-key))
        ; Transfer kUSD into the pool
        (free.kusd-usd.transfer depositorAccount poolAccount depositAmount))
        (with-read pool-state (pool-key)
          { "totalDeposits":=    existingTotalDeposits
          , "cumulativeGain":=   poolCumulativeGain }
          (update pool-state (pool-key)
            { "totalDeposits": (+ existingTotalDeposits depositAmount) })
          ; Now record or update this depositor’s claim
          (with-default-read claims depositorAccount
            { "deposit":       0.0
            , "gainSnapshot":  poolCumulativeGain }
            { "deposit":= priorDeposit
            , "gainSnapshot":= _ }
            (if (<= priorDeposit 0.0)
              ; first-time deposit
              (insert claims depositorAccount
                { "deposit":       depositAmount
                , "gainSnapshot":  poolCumulativeGain })
              ; top-up existing deposit
              (update claims depositorAccount
                { "deposit":       (+ priorDeposit depositAmount)
                , "gainSnapshot":  poolCumulativeGain })))))
      "Deposit succeeded"))


; When a depositor withdraws kUSD from the Stability Pool, the contract:
; 1. Verifies the user has enough deposited kUSD in their claim.
; 2. Calculates the KDA yield earned since their last snapshot.
; 3. Updates the pool’s totalDeposits and the user’s claim (reducing deposit and resetting snapshot).
; 4. Transfers the requested kUSD back to the user.
; 5. If any KDA yield is due, transfers it to the user.
(defun withdraw:string (withdrawAmount:decimal)
  @doc "Withdraw deposited kUSD and any accumulated KDA gains"
  (let ((withdrawingAccount (read-msg 'sender))
        (poolAccount         (pool-principal (pool-key))))
    (enforce (> withdrawAmount 0.0) "Withdraw amount must be > 0")

    ; Load the depositor’s record: how much they put in, and their last gain snapshot
    (with-read claims withdrawingAccount
      { "deposit":=      userDeposit
      , "gainSnapshot":= userGainSnapshot }

      (enforce (>= userDeposit withdrawAmount) "Exceeds deposit")
      (with-capability (WITHDRAW)
        (with-capability (POOL-RESERVE (pool-key))
          ; Total deposits and cumulative gain per kUSD
          (with-read pool-state (pool-key)
            { "totalDeposits":=   currentTotalDeposits
            , "cumulativeGain":=  currentCumulativeGain }
            ;How much KDA the user earned since their snapshot
            (let* (
                   ; Gain per kUSD = new total gain minus the snapshot when they last deposited
                   (earnedGainPerKusd (- currentCumulativeGain userGainSnapshot))

                   ; Total KDA owed = gain per kUSD × amount withdrawing
                   (kdaToSend          (* earnedGainPerKusd withdrawAmount))
                   
                   ; Update state values for kUSD
                   (newUserDeposit     (- userDeposit withdrawAmount))
                   (newTotalDeposits   (- currentTotalDeposits withdrawAmount))

                   ; Check the pool has enough kUSD to give back
                   (poolUsdBalance     (free.kusd-usd.get-balance poolAccount)))

              (enforce (>= poolUsdBalance withdrawAmount) "Insufficient pool kUSD")
              ; Shrink pool’s total and the user’s deposit
              (update pool-state (pool-key)
                      { "totalDeposits": newTotalDeposits })
              (update claims withdrawingAccount
                      { "deposit":      newUserDeposit
                      , "gainSnapshot": currentCumulativeGain })

              (free.kusd-usd.transfer poolAccount withdrawingAccount withdrawAmount)
              ; If there’s earned KDA, send it and emit an event
              (if (> kdaToSend 0.0)
                  (do
                    (coin.transfer poolAccount withdrawingAccount kdaToSend)
                    (emit-event (WITHDRAW_EXECUTED withdrawingAccount withdrawAmount kdaToSend))
                    "Withdraw completed")
                  ;If no KDA earned, just emit an event with zero gain
                  (do
                    (emit-event (WITHDRAW_EXECUTED withdrawingAccount withdrawAmount 0.0))
                    "Withdraw completed")))))))))



; When a vault is liquidated, the Stability Pool absorbs its collateral by:
; 1. Verifying there is at least one deposit to absorb against.
; 2. Computing how much KDA gain each kUSD depositor should receive (gainIncrement).
; 3. Guarding against numeric overflow in the gain calculation.
; 4. Updating the pool’s cumulativeGain so future withdrawals include this new yield.
  (defun absorb-debt:string (kdaAmount:decimal)
    @doc "Absorb collateral from liquidated vessels into the pool"
    (with-capability (ABSORB-DEBT kdaAmount)
     (with-capability (POOL-RESERVE (pool-key))
      (with-read pool-state (pool-key)
        { "totalDeposits":= totalDeposits
        , "cumulativeGain":= cumulativeGain }
        (enforce (> totalDeposits 0.0) "No deposits to absorb debt")
        ;  he amount of KDA each 1 kUSD depositor should gain from this liquidation
        (let ((gainIncrement (/ kdaAmount totalDeposits)))
          ; Prevent runaway gains by capping at an astronomically high value (10^29),
          ; ensuring we never overflow our decimal precision
          (enforce (< gainIncrement (^ 10.0 (dec 30))) "Gain overflow"); 
          (update pool-state (pool-key)
            { "cumulativeGain": (+ cumulativeGain gainIncrement) })))))
    "Debt absorbed")

)

;Deployment: on first install (not upgrade)

(if (read-msg "upgrade")
  [
    "upgraded contract"
  ]
  [
    (create-table pool-state)
    (create-table claims)
    ;(init-pool)
  ]
)
