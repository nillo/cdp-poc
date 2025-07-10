(module dummy-token GOV 
    @doc "Dummy Token: A simple token contract for testing purposes"

    (defcap GOV () 
        true
    )
    
    (defcap KUSD_MINT:bool () 
        @doc "Capability to mint kUSD"
        true
    )

    (defcap KUSD_BURN:bool () 
        @doc "Capability to mint kUSD"
        true
    )

    (defun kusd-mint-guard:guard () 
        (create-capability-guard (KUSD_MINT))
    )
    
    (defun kusd-burn-guard:guard () 
        (create-capability-guard (KUSD_BURN))
    )

    (defun register-kusd-mint-guard:string ()
        (cdp.kusd-usd.register-mint-guard (kusd-mint-guard))
    )
    
    (defun register-kusd-burn-guard:string ()
        (cdp.kusd-usd.register-burn-guard (kusd-burn-guard))
    )
    
    (defun mint-kusd:bool (account:string guard:guard amount:decimal)
        @doc "Mint kUSD tokens"
        (with-capability (KUSD_MINT)
            (cdp.kusd-usd.mint account guard amount)
        )
    )

    (defun burn-kusd:bool (account:string amount:decimal)
        @doc "Burn kUSD tokens"
        (with-capability (KUSD_BURN)
            (cdp.kusd-usd.burn account amount)
        )
    )

)