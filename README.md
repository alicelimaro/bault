# Bault Vault Smart Contract

This Clarity smart contract implements a multi-asset vault for STX and SIP-010 tokens, featuring dynamic fees, tiered user rewards, admin controls, whitelist management, and emergency withdrawal functionality.

---

## Features

- **Multi-Asset Support:**  
  Supports STX and any SIP-010 token, with configurable allocation limits and decimals.

- **Tiered User System:**  
  Users are assigned tiers (basic, silver, gold, platinum) based on deposit volume, which affects fee discounts.

- **Dynamic Fees:**  
  Withdraw and performance fees are set in basis points and can be discounted by user tier.

- **Admin Controls:**  
  Admin can pause/unpause the contract, set fees, manage whitelist, enable emergency mode, and change admin or fee recipient.

- **Whitelist:**  
  Only whitelisted users can deposit and withdraw.

- **Emergency Withdrawals:**  
  Users can withdraw without fees if emergency mode is enabled.

- **Strategy Interface:**  
  Simulate staking assets and harvesting yield for extensibility.

- **Slippage Protection:**  
  Deposit and withdrawal functions support minimum share/asset requirements to protect users from slippage.

- **Vault Health Checks:**  
  Read-only functions allow monitoring of vault invariants and share/asset ratios.

---

## Usage

### Admin Functions

- `add-asset(token, decimals)`  
  Add a SIP-010 token as a supported asset.

- `remove-asset(token)`  
  Remove a supported asset.

- `set-admin(new-admin)`  
  Change the admin address.

- `pause()` / `unpause()`  
  Pause or unpause the contract.

- `set-base-withdraw-fee(fee-bps)`  
  Set the base withdraw fee (max 10%).

- `set-performance-fee(fee-bps)`  
  Set the performance fee (max 20%).

- `set-fee-recipient(recipient)`  
  Set the recipient for collected fees.

- `add-to-whitelist(user)` / `remove-from-whitelist(user)`  
  Add or remove a user from the whitelist.

- `set-emergency-mode(enabled)`  
  Enable or disable emergency mode.

---

### User Functions

- `deposit(amount)`  
  Deposit STX to the vault.

- `deposit-with-slippage(amount, min-shares)`  
  Deposit STX with minimum shares requirement.

- `deposit-token(token, amount)`  
  Deposit SIP-010 tokens to the vault.

- `deposit-token-with-slippage(token, amount, min-shares)`  
  Deposit SIP-010 tokens with minimum shares requirement.

- `withdraw(shares)`  
  Withdraw assets based on shares, with dynamic fees.

- `withdraw-with-slippage(shares, min-assets)`  
  Withdraw with minimum asset requirement.

- `withdraw-token(shares)`  
  Withdraw SIP-010 tokens based on shares.

- `emergency-withdraw(recipient)`  
  Withdraw all assets without fees (only in emergency mode).

- `transfer(to, shares)`  
  Transfer vault shares to another user.

---

### Strategy Functions

- `stake-into-strategy()`  
  Stake all assets into a strategy (admin only).

- `harvest-yield()`  
  Harvest simulated yield (admin only).

---

### Read-Only Functions

- `get-user-shares(user)`  
  Get user's share balance and deposit info.

- `get-user-tier(user)`  
  Get user's tier.

- `get-vault-info()`  
  Get vault stats, share price, and status.

- `get-asset-config(token)`  
  Get asset configuration.

- `get-asset-balance(token)`  
  Get vault balance for an asset.

- `is-whitelisted(user)`  
  Check if a user is whitelisted.

- `get-withdraw-fee-preview(user, shares)`  
  Preview withdraw fee for a user and share amount.

- `get-vault-health()`  
  Check vault invariants and ratios.

- `get-balance(user)`  
  Get user's share balance (legacy).

- `get-total-supply()`  
  Get total shares issued (legacy).

---

## Error Codes

- `ERR_UNAUTHORIZED` (u100): Caller is not admin.
- `ERR_PAUSED` (u101): Contract is paused.
- `ERR_NOT_WHITELISTED` (u102): User not whitelisted.
- `ERR_INSUFFICIENT_BALANCE` (u103): Insufficient balance.
- `ERR_INSUFFICIENT_SHARES` (u104): Insufficient shares.
- `ERR_INVALID_AMOUNT` (u105): Invalid amount.
- `ERR_TRANSFER_FAILED` (u106): Transfer failed.
- `ERR_ASSET_NOT_FOUND` (u107): Asset not found.
- `ERR_NOT_EMERGENCY` (u108): Emergency mode not enabled.
- `ERR_INVALID_TOKEN` (u1001): Invalid token.
- `ERR_INVALID_RECIPIENT` (u1002): Invalid recipient.
- `ERR_SELF_TRANSFER` (u1003): Cannot transfer to self.
- `ERR_ASSET_EXISTS` (u1004): Asset already exists.
- `ERR_ASSET_DISABLED` (u1005): Asset is disabled.
- `ERR_SLIPPAGE_EXCEEDED` (u1006): Slippage protection triggered.
- `ERR_VAULT_INVARIANT` (u1007): Vault invariant check failed.

---

## Notes

- All deposits and withdrawals require the user to be whitelisted.
- Emergency withdrawals are only possible when emergency mode is enabled.
- Tier upgrades are automatic based on deposit volume.
- Asset allocations and decimals must be set correctly for SIP-010 tokens.
- Slippage protection is available for deposits and withdrawals.
- Vault health and invariants can be monitored via read-only functions.

---

## License

MIT License (see repository for details).
