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
  Admin can pause/unpause the contract, set fees, manage whitelist, enable emergency mode, and upgrade the contract.

- **Whitelist:**  
  Only whitelisted users can deposit and withdraw.

- **Emergency Withdrawals:**  
  Users can withdraw without fees if emergency mode is enabled.

- **Strategy Interface:**  
  Simulate staking assets and harvesting yield for extensibility.

---

## Usage

### Admin Functions

- `add-supported-asset(token, max-allocation, decimals)`  
  Add a SIP-010 token as a supported asset.

- `toggle-asset(token, enabled)`  
  Enable or disable a supported asset.

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

- `update-tier-discount(tier, withdraw-discount, performance-discount, volume-threshold)`  
  Update tier-based fee discounts and thresholds.

- `update-whitelist(user, status)`  
  Add or remove a user from the whitelist.

- `enable-emergency()` / `disable-emergency()`  
  Enable or disable emergency mode.

- `upgrade(new-contract)`  
  Signal contract upgrade.

---

### User Functions

- `deposit(amount)`  
  Deposit STX to the vault.

- `deposit-token(token, amount)`  
  Deposit SIP-010 tokens to the vault.

- `withdraw(user-shares)`  
  Withdraw assets based on shares, with dynamic fees.

- `emergency-withdraw()`  
  Withdraw all assets without fees (only in emergency mode).

---

### Strategy Functions

- `stake-into-strategy()`  
  Stake all assets into a strategy (admin only).

- `harvest-yield()`  
  Harvest simulated yield (admin only).

- `simulate-yield(amount)`  
  Set simulated yield amount (admin only).

- `rebalance(to-strategy)`  
  Switch strategy (admin only).

---

### Read-Only Functions

- `get-user-shares(user)`  
  Get user's share balance.

- `get-user-tier-info(user)`  
  Get user's tier and deposit volume.

- `get-asset-config(token)`  
  Get asset configuration.

- `get-asset-balance(token)`  
  Get vault balance for an asset.

- `get-total-assets()`  
  Get total assets in the vault.

- `get-total-shares()`  
  Get total shares issued.

- `get-share-price()`  
  Get current share price.

- `get-fee-info()`  
  Get current fee settings.

- `get-user-withdraw-fee-preview(user, amount)`  
  Preview withdraw fee for a user.

- `is-paused()`  
  Check if contract is paused.

- `is-emergency()`  
  Check if emergency mode is enabled.

- `get-admin()`  
  Get current admin address.

- `is-whitelisted(user)`  
  Check if a user is whitelisted.

---

## Error Codes

- `ERR-NO-FUNDS` (u100): No funds provided.
- `ERR-NO-SHARES` (u101): Insufficient shares.
- `ERR-PAUSED` (u102): Contract is paused.
- `ERR-NOT-ADMIN` (u103): Caller is not admin.
- `ERR-INVALID-FEE` (u104): Invalid fee value.
- `ERR-NOT-WHITELISTED` (u105): User not whitelisted.
- `ERR-NOT-EMERGENCY` (u106): Emergency mode not enabled.
- `ERR-ASSET-NOT-SUPPORTED` (u107): Asset not supported.
- `ERR-ASSET-DISABLED` (u108): Asset is disabled.
- `ERR-MAX-ALLOCATION-EXCEEDED` (u109): Max allocation exceeded.
- `ERR-INVALID-TOKEN` (u110): Invalid token.

---

## Notes

- All deposits and withdrawals require the user to be whitelisted.
- Emergency withdrawals are only possible when emergency mode is enabled.
- Tier upgrades are automatic based on deposit volume.
- Asset allocations and decimals must be set correctly for SIP-010 tokens.

---

## License

MIT License (see repository for details).
