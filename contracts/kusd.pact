(namespace (read-msg "ns"))

(module kusd-usd GOVERNANCE

  @doc " 'KUSD' is an implementation of the KUSD Stablecoin. \ 
         \ This module implements the fungible-v2 interface and supports features for \
         \ minting, burning, freezing, and unfreezing accounts, as well as transfers \ 
         \ and cross-chain transfers."

  (implements fungible-v2)
  (implements fungible-xchain-v1)

  ; --------------------------------------------------------------------------
  ; Keysets

  (defconst ADMIN_KS:string "kusd.admin-keyset")
  (defconst FREEZE_MANAGER_KS:string "kusd.freeze-manager-keyset")

  ; --------------------------------------------------------------------------
  ; Metadata

  (defconst TOKEN_NAME:string "KUSD")
  (defconst TOKEN_SYMBOL:string "kUSD")

  ; --------------------------------------------------------------------------
  ; Constants

  (defconst MINIMUM_PRECISION 12
    "Minimum allowed precision for coin transactions")

  (defconst VALID_CHAIN_IDS (map (int-to-str 10) (enumerate 0 19))
    "List of all valid Chainweb chain ids")

  (defconst DEFAULT_ROW:string "" "Used for tables with one row")
  (defconst MINT_ROW:string "mint" "Used for tables to store mint guards")
  (defconst BURN_ROW:string "burn" "Used for tables to store burn guards")

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema token-info-schema
    @doc "Saves token metadata information"
    name:string
    symbol:string
    supply:decimal
  )

  (defschema token-schema
    @doc " An account, holding a token balance. \
         \ \
         \ ROW KEY: accountId. "
    balance:decimal
    guard:guard
    frozen:bool
  )

  (defschema account-details-schema
   account:string
   balance:decimal
   guard:guard
   frozen:bool
  )

  (defschema capability-guard-schema
   guard:guard
  )

  (deftable token-info-table:{token-info-schema})
  (deftable token-table:{token-schema})
  (deftable capability-guard-table:{capability-guard-schema})

  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE:bool ()
    @doc "Give the admin full access to call and upgrade the module. "
    (enforce-guard ADMIN_KS)
    true
  )

  (defcap FREEZE_MANAGER:bool ()
    @doc "Capability for managing freeze operations"
    (enforce-guard FREEZE_MANAGER_KS)
    true
  )

  (defcap DEBIT:bool (sender:string)
    @doc "Capability for managing debiting operations"
    (with-read token-table sender {
        "guard" := g      
      }
      (enforce-principal sender g)
      (enforce-guard g))
    true
  )

  (defcap CREDIT:bool (receiver:string)
    @doc "Capability for managing crediting operations"
    (enforce (!= receiver "") "Invalid receiver")
    true
  )

  (defcap SUPPLY:bool ()
    @doc "Capability for managing supply changing operations (minting / burning"
    true
  )

  (defcap TRANSFER:bool
    ( sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce-valid-transfer sender receiver amount)
    (compose-capability (DEBIT sender))
    (compose-capability (CREDIT receiver))
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )

    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed]))
      newbal)
  )

  (defcap TRANSFER_XCHAIN:bool
    ( sender:string
      receiver:string
      amount:decimal
      target-chain:string
    )
    @managed amount TRANSFER_XCHAIN-mgr
    (enforce-valid-transfer-xchain sender receiver amount target-chain)
    (compose-capability (DEBIT sender))
  )

  (defun TRANSFER_XCHAIN-mgr:decimal
    ( managed:decimal
      requested:decimal
    )

    (enforce (>= managed requested)
      (format "TRANSFER_XCHAIN exceeded for balance {}" [managed]))
    0.0
  )

  (defcap TRANSFER_XCHAIN_RECD:bool
    ( sender:string
      receiver:string
      amount:decimal
      source-chain:string
    )
    @event
    true
  )

  (defcap SUPPLY_UPDATED:bool (supply:decimal)
    @doc "Event emitted when supply is changed."
    @event
    true
  )

  (defcap BURN:bool (amount:decimal from-account:string)
    @doc "Event emitted when tokens are burned."
    @event
    true
  )

  (defcap MINT:bool (amount:decimal to-account:string)
    @doc "Event emitted when tokens are minted."
    @event
    true
  )

  (defcap ACCOUNT_FROZEN:bool (account:string)
    @doc "Event emitted when an account is frozen."
    @event
    true
  )

  (defcap ACCOUNT_UNFROZEN:bool (account:string )
    @doc "Event emitted when an account is unfrozen."
    @event
    true
  )

  ; --------------------------------------------------------------------------
  ; Utilities

  (defun init:string ()
    (with-capability (GOVERNANCE)
      (insert token-info-table DEFAULT_ROW {
        "name": TOKEN_NAME,
        "symbol": TOKEN_SYMBOL,
        "supply": 0.0
      })
    )
  )
  
  (defun register-cdp-mint-guard:string (guard:guard)
    @doc "Register the guard to protect the mint operation."
    (with-capability (GOVERNANCE) ;; TODO: should we protect with a different capability? 
      (insert capability-guard-table MINT_ROW
        { "guard": guard }
      )
    )
  )

  (defun register-cdp-burn-guard:string (guard:guard)
    @doc "Register the guard to protect the burn operation."
    (with-capability (GOVERNANCE) ;; TODO: should we protect with a different capability? 
      (insert capability-guard-table BURN_ROW
        { "guard": guard }
      )
    )
  )

  (defun enforce-cdp-mint:bool ()
    (with-read capability-guard-table MINT_ROW {'guard:=g}
      (enforce-guard g)
    )
  )

  (defun enforce-cdp-burn:bool ()
    (with-read capability-guard-table BURN_ROW {'guard:=g}
      (enforce-guard g)
    )
  )

  (defun enforce-unit:bool (amount:decimal)
    @doc "Enforce minimum precision allowed for KUSD transactions"
    (enforce
      (= (floor amount MINIMUM_PRECISION)
         amount)
      (format "Amount violates minimum precision: {}" [amount]))
  )

  (defun enforce-valid-amount:bool (amount:decimal)
    @doc "Enforce that the amount is positive and has the correct precision"
    (enforce-unit amount) 
    (enforce (> amount 0.0) "Amount must be positive")
  )

  (defun enforce-valid-transfer:bool (sender:string receiver:string amount:decimal)
    @doc "Enforce that the accounts are not faulty and that the amount is valid"
    (enforce (!= sender receiver) "Sender cannot be the receiver of a transfer")
    (enforce (!= sender "") "Invalid sender")
    (enforce (!= receiver "") "Invalid receiver")
    (enforce-valid-amount amount)
  )

  (defun enforce-valid-transfer-xchain:bool 
    ( sender:string
      receiver:string
      amount:decimal
      target-chain:string
    )
    @doc "Enforce that the accounts and target chains are not faulty and that the amount is valid"
    (enforce (!= sender "") "Invalid sender")
    (enforce (!= receiver "") "Invalid receiver")
    (enforce-valid-target-chain target-chain)
    (enforce-valid-amount amount)
  )

  (defun enforce-valid-target-chain:bool (target-chain:string)
    @doc "Enforce that the target chain is valid"
    (enforce (!= "" target-chain) "empty target-chain")
    (enforce (!= (at 'chain-id (chain-data)) target-chain)
      "cannot run cross-chain transfers to the same chain")
    (enforce (contains target-chain VALID_CHAIN_IDS)
      "target chain is not a valid chainweb chain id")
  )

  (defun get-metadata:object{token-info-schema} ()
    (read token-info-table DEFAULT_ROW))

  (defun enforce-principal:bool (name:string guard:guard)
    @doc "Enforce that a particular name/guard pair form a principal name"
    (enforce
      (validate-principal guard name)
      "Principal/guard name mismatch: all account names are required to be valid principals")
  )

  ; --------------------------------------------------------------------------
  ; Fungible-v2 Implementation

  (defun create-account:string (account:string guard:guard)
    (enforce-principal account guard)

    (insert token-table account
      { "balance" : 0.0
      , "guard"   : guard
      , "frozen"  : false
      })
  )

  (defun get-balance:decimal (account:string)
    (with-read token-table account
      { "balance" := balance }
      balance
    )
  )

  (defun details:object{fungible-v2.account-details} (account:string)
    (remove "frozen" (kusd-account-details account))
  )

  (defun kusd-account-details:object{account-details-schema} (account:string)
    (+ { "account" : account }  (read token-table account))
  )

  (defun rotate:string (account:string new-guard:guard)
    (enforce false "It is unsafe for principal accounts to rotate their guard")
    ""
  )

  (defun precision:integer
    ()
    MINIMUM_PRECISION)

  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer AMOUNT from SENDER to RECEIVER. Fails if the RECEIVER account \ 
        \ does not exist"
    (with-read token-table receiver
      { "guard" := g }
      (transfer-create sender receiver g amount)
    )
  )

  (defun transfer-create:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal )
    @doc "Transfer AMOUNT from SENDER to RECEIVER. Creates the RECEIVER account \
        \ if it does not exist"
    (enforce-principal receiver receiver-guard)

    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (credit receiver receiver-guard amount))
  )

  (defun debit:string (account:string amount:decimal)
    @doc "Debit AMOUNT from ACCOUNT balance"
    (enforce-valid-amount amount)

    (require-capability (DEBIT account))
    (with-read token-table account
      { "balance" := balance,
        "frozen" := frozen }

      (enforce (<= amount balance) "Insufficient funds")
      (enforce (not frozen)
        "only active or new accounts may be debited")

      (update token-table account
        { "balance" : (- balance amount) }
        ))
  )

  (defun credit:string (account:string guard:guard amount:decimal)
    @doc "Credit AMOUNT to ACCOUNT balance"
    (enforce-valid-amount amount)

    (require-capability (CREDIT account))
    (with-default-read token-table account
      { "balance" : -1.0, "guard" : guard, "frozen" : false }
      { "balance" := balance, "guard" := retg, "frozen" := frozen }
      ; we don't want to overwrite an existing guard with the user-supplied one
      (enforce (= retg guard)
        "Account guards do not match")

      (if (= balance -1.0)
        (do
          (enforce-principal account guard)
          (insert token-table account
             {   "balance": amount,
                 "guard": guard,
                "frozen": false
             }))
        (update token-table account {
          "balance": (+ amount balance)
        })
      ))
  )

  (defschema crosschain-schema
    @doc "Schema for yielded value in cross-chain transfers"
    receiver:string
    receiver-guard:guard
    amount:decimal
    source-chain:string)

  (defpact transfer-crosschain:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    @doc "Transfer AMOUNT from SENDER from SOURCE_CHAIN to RECEIVER on TARGET-CHAIN. \
         \ The RECEIVER account will be created if it does not exist."
    (step
      (with-capability
        (TRANSFER_XCHAIN sender receiver amount target-chain)
        
        (enforce-principal receiver receiver-guard)

        ;; step 1 - debit delete-account on current chain
        (debit sender amount)
        (emit-event (TRANSFER sender "" amount))
        ;; deduct amount that is being transferred from supply on current chain
        (with-capability (SUPPLY)
          (update-supply (- amount))
        )

        (let
          ((crosschain-details:object{crosschain-schema}
            { "receiver" : receiver
            , "receiver-guard" : receiver-guard
            , "amount" : amount
            , "source-chain" : (at 'chain-id (chain-data))
            }))
          (yield crosschain-details target-chain)
          )))

    (step
      (resume
        { "receiver" := receiver
        , "receiver-guard" := receiver-guard
        , "amount" := amount
        , "source-chain" := source-chain
        }

        (emit-event (TRANSFER "" receiver amount))
        (emit-event (TRANSFER_XCHAIN_RECD "" receiver amount source-chain))

        ;; step 2 - credit create account on target chain
        (with-capability (CREDIT receiver)
          (credit receiver receiver-guard amount))

        ;; increase amount that is being transferred to supply on target chain
        (with-capability (SUPPLY)
          (update-supply amount)
        ))
      )
    )


  ; --------------------------------------------------------------------------
  ; Minting and Burning

  (defun mint:bool (to:string amount:decimal)
    @doc "Mints amount of token to an account"
    (enforce-cdp-mint)
    (with-capability (CREDIT to)
      (credit to (at 'guard (details to)) amount)
      (emit-event (MINT amount to))
    )
    (with-capability (SUPPLY)
      (update-supply amount)
    )
  )

  (defun burn:bool (from:string amount:decimal)
    @doc "Burns amount of tokens from an account"
    (enforce-cdp-burn)
    (with-capability (DEBIT from)
      (debit from amount)
      (emit-event (BURN amount from))
    )
    (with-capability (SUPPLY)
      (update-supply (- amount))
    )
  )

  (defun update-supply:bool (amount:decimal)
    @doc "Updates total supply at mint or burn"
    (require-capability (SUPPLY))
    (let
      (
        (supply (at 'supply (read token-info-table DEFAULT_ROW)))
        (new-supply (+ supply amount))
      )

      (enforce (>= new-supply 0.0) "Incorrect supply amount, supply must be greater than 0")

      (update token-info-table DEFAULT_ROW {'supply: new-supply })
      (emit-event (SUPPLY_UPDATED new-supply)))
  )

  ; --------------------------------------------------------------------------
  ; Freezing and Unfreezing

  (defun freeze-account:string (account:string)
    @doc "Freeze an account"
    (with-capability (FREEZE_MANAGER)
      (emit-event (ACCOUNT_FROZEN account))
      (update token-table account {"frozen": true})
    )
  )

  (defun unfreeze-account:string (account:string )
    @doc "Unfreeze an account"
    (with-capability (FREEZE_MANAGER)
      (emit-event (ACCOUNT_UNFROZEN account))
      (update token-table account {"frozen": false})
    )
  )
)

(if (read-msg "upgrade")
  [
    "upgraded contract"
  ]
  [
    (create-table token-table)
    (create-table token-info-table)
    (create-table capability-guard-table)
    (init)
  ]
)

(enforce-guard ADMIN_KS)