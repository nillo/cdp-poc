(namespace (read-msg "ns"))

(module kusd-usd GOVERNANCE

  @doc " 'KUSD' is an implementation of the KUSD Stablecoin. \ 
         \ This module implements the fungible-v2 interface and supports features for \
         \ minting, burning accounts, as well as transfers and cross-chain transfers."

  (implements fungible-v2)
  (implements fungible-xchain-v1)

  ; --------------------------------------------------------------------------
  ; Keysets

  (defconst ADMIN_KS:string "kusd.admin-keyset")

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
  
  (defconst VALID_CHAIN_IDS_MINT:[string] ["2"])
  (defconst VALID_CHAIN_IDS_BURN:[string] ["2"])

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

  (defschema supply-schema
    @doc "Saves token supply information"
    supply:decimal
  )

  (defschema token-schema
    @doc " An account, holding a token balance. \
         \ \
         \ ROW KEY: accountId. "
    balance:decimal
    guard:guard
  )

  (defschema account-details-schema
    account:string
    balance:decimal
    guard:guard
  )

  (defschema capability-guard-schema
    guard:guard
  )

  (deftable supply-table:{supply-schema})
  (deftable token-table:{token-schema})
  (deftable capability-guard-table:{capability-guard-schema})

  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE:bool ()
    @doc "Give the admin full access to call and upgrade the module. "
    (enforce-guard ADMIN_KS)
    true
  )

  (defcap DEBIT:bool (sender:string amount:decimal)
    @doc "Capability for managing debiting operations"
    (with-read token-table sender {
        "balance":= balance
      , "guard" := g      
      }
      (enforce-valid-amount amount)
      (enforce-principal sender g)
      (enforce (<= amount balance) "Insufficient funds")
    )
    true
  )

  (defcap CREDIT:bool (receiver:string amount:decimal)
    @doc "Capability for managing crediting operations"
    (enforce-valid-amount amount)

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
    (with-read token-table sender {
        "guard" := g      
      }
      (enforce-principal sender g)
      (enforce-guard g)
    )
    (enforce-valid-transfer sender receiver amount)
    (compose-capability (DEBIT sender amount))
    (compose-capability (CREDIT receiver amount))
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
    (with-read token-table sender {
        "guard" := g      
      }
      (enforce-principal sender g)
      (enforce-guard g)
    )
    (enforce-valid-transfer-xchain sender receiver amount target-chain)
    (compose-capability (DEBIT sender amount))
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

  (defcap REGISTER_MINT_GUARD:bool (guard:guard)
    @doc "Capability to register a mint guard"
    @event
    (compose-capability (GOVERNANCE))
  )

  (defcap REGISTER_BURN_GUARD:bool (guard:guard)
    @doc "Capability to register a burn guard"
    @event
    (compose-capability (GOVERNANCE))
  )
  
  (defcap SUPPLY_UPDATED:bool (supply:decimal)
    @doc "Event emitted when supply is changed."
    @event
    true
  )

  (defcap MINT:bool (account:string amount:decimal)
    @doc "Event emitted when tokens are minted."
    (enforce-mint-guard)
    (enforce-valid-mint-chain)
    (compose-capability (CREDIT account amount))
    (compose-capability (SUPPLY))
    (emit-event (TRANSFER "" account amount))
  )

  (defcap BURN:bool (account:string amount:decimal)
    @doc "Event emitted when tokens are burned."
    (enforce-burn-guard)
    (enforce-valid-burn-chain)
    (compose-capability (DEBIT account amount))
    (compose-capability (SUPPLY))
    (emit-event (TRANSFER account "" amount))
  )

  ; --------------------------------------------------------------------------
  ; Utilities
  
  (defun register-mint-guard:string (guard:guard)
    @doc "Register the guard to protect the mint operation."
    (enforce-valid-mint-chain)
    (with-capability (REGISTER_MINT_GUARD guard)
      (insert capability-guard-table MINT_ROW
        { "guard": guard }
      )
    )
  )

  (defun register-burn-guard:string (guard:guard)
    @doc "Register the guard to protect the burn operation."
    (enforce-valid-burn-chain)
    (with-capability (REGISTER_BURN_GUARD guard)
      (insert capability-guard-table BURN_ROW
        { "guard": guard }
      )
    )
  )

  (defun enforce-mint-guard:bool ()
    (with-read capability-guard-table MINT_ROW {'guard:=g}
      (enforce-guard g)
    )
  )

  (defun enforce-burn-guard:bool ()
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

  (defun enforce-valid-mint-chain:bool ()
    @doc "Enforce that the target chain is valid for minting"
    (enforce (contains (at 'chain-id (chain-data)) VALID_CHAIN_IDS_MINT)
      (format "Can only mint on chains {}" [VALID_CHAIN_IDS_MINT]))
  )

  (defun enforce-valid-burn-chain:bool ()
    @doc "Enforce that the target chain is valid for burning"
    (enforce (contains (at 'chain-id (chain-data)) VALID_CHAIN_IDS_BURN)
      (format "Can only burn on chains {}" [VALID_CHAIN_IDS_BURN]))
  )

  (defun supply:decimal ()
    (with-default-read supply-table DEFAULT_ROW
      { "supply": 0.0 }
      { "supply":= supply }
      supply
    )
  )

  (defun get-metadata:object{token-info-schema} ()
    {
      "name": TOKEN_NAME,
      "symbol": TOKEN_SYMBOL,
      "supply": (supply)
    }
  )

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
      })
  )
  
  (defun get-balance:decimal (account:string)
    (with-read token-table account
      { "balance" := balance }
      balance
    )
  )

  (defun details:object{fungible-v2.account-details} (account:string)
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
      (credit receiver receiver-guard amount)
    )
  )

  (defun debit:string (account:string amount:decimal)
    @doc "Debit AMOUNT from ACCOUNT balance"
    (require-capability (DEBIT account amount))
    (update token-table account
      { "balance" : (- (get-balance account) amount) }
    )
  )

  (defun credit:string (account:string guard:guard amount:decimal)
    @doc "Credit AMOUNT to ACCOUNT balance"
    (require-capability (CREDIT account amount))
    (with-default-read token-table account
      { "balance" : 0.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      (enforce (= retg guard) "Account guards do not match")
      (enforce-principal account guard)
      (write token-table account
        {   "balance": (+ amount balance),
            "guard": guard
        })
    )
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
        (with-capability (CREDIT receiver amount)
          (credit receiver receiver-guard amount))

        ;; increase amount that is being transferred to supply on target chain
        (with-capability (SUPPLY)
          (update-supply amount)
        ))
      )
    )


  ; --------------------------------------------------------------------------
  ; Minting and Burning

  (defun mint:bool (to:string guard:guard amount:decimal)
    @doc "Mints amount of token to an account"
    (with-capability (MINT to amount)
      (credit to guard amount)
      (update-supply amount)
    ) 
  )

  (defun burn:bool (from:string amount:decimal)
    @doc "Burns amount of tokens from an account"
    (with-capability (BURN from amount)
      (debit from amount)
      (update-supply (- amount))
    )
  )

  (defun update-supply:bool (amount:decimal)
    @doc "Updates total supply at mint or burn"
    (require-capability (SUPPLY))
    (let ((new-supply:decimal (+ (supply) amount)))
      (enforce (>= new-supply 0.0) "Incorrect supply amount, supply must be greater than 0")
      (write supply-table DEFAULT_ROW {'supply: new-supply })
      (emit-event (SUPPLY_UPDATED new-supply)))
  )
)

(if (read-msg "upgrade")
  [
    "upgraded contract"
  ]
  [
    (create-table token-table)
    (create-table supply-table)
    (create-table capability-guard-table)
  ]
)

(enforce-guard ADMIN_KS)