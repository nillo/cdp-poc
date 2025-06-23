; The Price feed module contains a set of oracles refs for fetching prices of assets based on the oracle provider.
; optionally fallback oracles can also be registered in case the primary source fails or is stale.


(namespace (read-msg "ns"))
(enforce-guard (keyset-ref-guard kudos-addresses.ADMIN_KEYSET))

(module kudos-price-feed GOVERNANCE

  (use kudos-addresses)
  (implements price-feed-iface)

  ; ----------------------------------------------------------------------
  ; Constants

  (defconst NIL "")
  (defconst UNIX_EPOCH (parse-time "%s" "0") "Zero Unix epoch")

  ; ----------------------------------------------------------------------
  ; Caps

  (defcap GOVERNANCE:bool ()
    (enforce-guard (keyset-ref-guard ADMIN_KEYSET)))

  (defcap ADMIN:bool ()
    @doc "Only allows admin to call"
    (enforce-guard (keyset-ref-guard ADMIN_KEYSET)))

  (defcap INTERNAL:bool () true)

  (defcap NEW_ORACLE_REGISTERED (
    asset:string
    oracle:string
    is-fallback:bool
  )
    @doc "Event emitted when new oracle is registered"
    @event true)

  ; ----------------------------------------------------------------------
  ; Schemas

  (defschema value-schema
    timestamp:time
    value:decimal
    timeout:decimal)

  (defschema ref
    timelock-ref:module{timelock-iface}
    initialized:bool)

  (deftable oracle-table:{price-feed-iface.oracle})
  (deftable fallback-table:{price-feed-iface.fallback})
  (deftable refs-table:{ref})

  ; ----------------------------------------------------------------------
  ; Functions

  (defun init (asset:string oracle:string timeout:decimal timelock-ref:module{timelock-iface})
    (with-capability (ADMIN)
      (insert refs-table REF_KEY { "initialized": false, "timelock-ref": timelock-ref })
      (set-oracle asset oracle timeout false))
      (update refs-table REF_KEY { "initialized": true }))

  (defun set-oracle:bool (asset:string oracle:string timeout:decimal is-fallback:bool)
    (with-capability (ADMIN)
      (with-capability (INTERNAL)
        (let ((target:string (+ PRICE_FEED_MODULE ".set-oracle")))
          (timelock-check target (hash { "target": target, "asset": asset, "oracle": oracle, "timeout": timeout, "is-fallback": is-fallback }))))
        (if is-fallback
          (let ((_ 0))
            (with-default-read oracle-table asset { "provider": NIL } { "provider" := provider}
              (enforce (!= provider NIL) "Existing oracle required for fallback"))
            (insert fallback-table asset { "provider": oracle, "timeout": timeout }))
          (let ((_ 0))
            (insert oracle-table asset { "provider": oracle, "timeout": timeout })
            (with-capability (INTERNAL)
              (let ((result (fetch-oracle-price asset)))
                (enforce (!= 0.0 (at 'value result)) "Invalid Oracle response")))))
        (emit-event (NEW_ORACLE_REGISTERED asset oracle is-fallback))))

  (defun fetch-price:decimal (asset:string)
    @doc "Fetches the price for an asset from a previosly configured oracle.\
          \Callers:\
            \- borrower-operations.open-vessel()\
            \- borrower-operations.adjust-vessel()\
            \- borrower-operations.close-vessel()\
            \- vessel-manager.liquidate-vessels()\
            \- vessel-manager.batch-liquidate-vessels()\
            \- vessel-manager.redeem-collateral()"

    (with-capability (INTERNAL)
      (let ((result (fetch-oracle-price asset)))
        ; TODO: This should be an If instead of a enforce
        ; If the value was 0.0 or stale we should call the fallback oracle.
        (enforce (and (not (is-stale-price result)) (!= 0.0 (at 'value result))) "Oracle response is stale or 0.0")
        (at 'value result))))

  (defun update-asset-timeout:string (asset:string new-timeout:decimal)
    (with-capability (ADMIN)
      (update oracle-table asset { "timeout": new-timeout })))

  ; ----------------------------------------------------------------------
  ; Internal functions

  (defun fetch-oracle-price:object{value-schema} (asset:string)
    (require-capability (INTERNAL))
    (with-default-read oracle-table asset { "provider": NIL, "timeout": 0.0 } { "provider" := provider, "timeout" := timeout }
      (cond
        ((= "DIA" provider)
          (let ((result (free.dia-oracle.get-value (+ asset "/USD"))))
            { "timestamp": (at 'timestamp result), "value": (at 'value result), "timeout": timeout }))
        { "timestamp": UNIX_EPOCH, "value": 0.0, "timeout": 0.0 })))

  (defun is-stale-price:bool (result:object{value-schema})
    (let (
      (timestamp (at 'timestamp result))
      (timeout (at 'timeout result))
      (now (at 'block-time (chain-data))))
      (> (diff-time now timestamp) timeout)))

  (defun timelock-check:bool (target:string t-hash:string)
    @doc "If the contract is initialized, calls the timelock contract to check if the function can be executed. \
    \It fails if the function can't be executed. \
    \If not initialized, it allows the admin to execute the function right away"
    (require-capability (INTERNAL))
    (with-read refs-table REF_KEY
      { "timelock-ref":= timelock-ref:module{timelock-iface},
        "initialized":= initialized:bool }
      (if initialized
        (timelock-ref::execute-transaction target t-hash)
        true)))
)

(if (read-msg "init") [
  (create-table refs-table)
  (create-table oracle-table)
  (create-table fallback-table)
  ]
  "Upgrade complete")
