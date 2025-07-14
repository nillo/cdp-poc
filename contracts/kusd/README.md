# KUSD Stablecoin

KUSD is a Stablecoin in the Kudos Protocol

## User Functions
 
### `create-account`

Creates account with supplied guard 

**Inputs**  
- `account: string`
- `guard: guard`

### `register-mint-guard`

Registers a burn guard that restricts token burn operations.
Only authorized contracts can burn tokens by registering a capability guard.
Requires the GOVERNANCE signature and is limited to designated chains.

**Inputs**  
- `account: string`
- `guard: guard`

**Events Emitted**
- `(REGISTER_MINT_GUARD guard) `

### `register-burn-guard`

Registers a burn guard that restricts token burn operations.
Only authorized contracts can burn tokens by registering a capability guard.
Requires the GOVERNANCE signature and is limited to designated chains.

**Inputs**  
- `account: string`
- `guard: guard`

**Events Emitted**
- `(REGISTER_BURN_GUARD guard) `

### `mint`

Mints `amount` of token to `account`. Creates the account if does not exist. The registered mint-guard is enforced. 

**Inputs**  
- `account: string`
- `guard: guard`
- `amount: decimal`

**Events Emitted**
- `(TRANSFER "" account amount) `
- `(SUPPLY_UPDATED new-supply)`

### `burn`

Burns `amount` of token from the `account`. The registered burn-guard is enforced. 

**Inputs**  
- `account: string`
- `amount: decimal`

**Events Emitted**
- `(TRANSFER account "" amount)` 
- `(SUPPLY_UPDATED new-supply)`

### `transfer`
Transfers `amount` of token from the `sender` to the `receiver`. The sender's guard is enforced. Fails if `receiver` account does not exist

**Inputs**  
- `sender: string`
- `receiver: string`
- `amount: decimal`

**Events Emitted**
- `(TRANSFER sender receiver amount)`

### `transfer-create`
Transfers `amount` of token from the `sender` to the `receiver`. The sender's guard is enforced. Creates `receiver` account if it does not exist. 

**Inputs**  
- `sender: string`
- `receiver: string`
- `amount: decimal`

**Events Emitted**
- `(TRANSFER sender receiver amount)`

### `transfer-crosschain`
TODO

## Getter Functions

### `get-metadata`
Returns `name`, `symbol`, `supply` of the token

### `supply`
Returns `supply` of the token

### `get-balance`
Returns the `balance` of the account

**Inputs**  
- `account: string`

### `details`
Returns the `account`, `guard`, `balance` of the account

**Inputs**  
- `account: string`

### `precision`

Returns the `precision` of the token