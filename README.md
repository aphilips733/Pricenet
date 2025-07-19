# Pricenet - Decentralized Price Board

A Clarity smart contract for tracking and verifying local market prices in a decentralized manner.

## Overview

Pricenet enables users to submit, vote on, and verify current market prices for various items across different locations. The system uses a staking mechanism to ensure data quality and implements a reputation system for price reporters.

## Features

- **Price Submission**: Submit current market prices for items at specific locations
- **Voting System**: Community voting on price accuracy with stake-weighted votes
- **Price Verification**: Automatic verification based on community consensus
- **Reputation System**: Track reporter reliability and verification rates
- **Staking Mechanism**: Stake tokens to participate in price reporting
- **Cooldown System**: Prevent spam with submission cooldowns
- **Historical Data**: Access to price history and trends

## Contract Functions

### Public Functions

#### `submit-price (item location price)`
Submit a new price for an item at a specific location.
- Requires minimum stake amount
- Subject to cooldown period between submissions
- Records price history automatically

#### `vote-on-price (item location vote-for stake)`
Vote on the accuracy of a submitted price.
- `vote-for`: true for accurate, false for inaccurate
- `stake`: amount of tokens to stake on the vote
- Cannot vote twice on the same price

#### `verify-price (item location)`
Verify a price based on community votes.
- Prices are verified when votes-for > votes-against
- Updates reporter reputation scores

#### `stake-tokens (amount)`
Stake tokens to participate in price reporting.
- Required to submit prices and vote
- Minimum stake amount set by contract owner

#### `withdraw-stake (amount)`
Withdraw staked tokens.
- Cannot withdraw more than current stake

### Read-Only Functions

#### `get-price (item location)`
Get the current price data for an item at a location.

#### `get-verified-price (item location)`
Get only verified price data.

#### `get-price-history (item location entry-id)`
Access historical price data.

#### `get-reporter-reputation (reporter)`
Get reputation stats for a price reporter.

#### `can-submit-price (reporter item location)`
Check if a reporter can submit a price (cooldown status).

## Usage Examples

### Submit a Price
```clarity
(contract-call? .pricenet submit-price "apples" "new-york" u250)
```

### Vote on a Price
```clarity
(contract-call? .pricenet vote-on-price "apples" "new-york" true u50)
```

### Get Current Price
```clarity
(contract-call? .pricenet get-price "apples" "new-york")
```

### Stake Tokens
```clarity
(contract-call? .pricenet stake-tokens u100)
```

## Contract Parameters

- **Minimum Stake**: Default 100 tokens
- **Cooldown Period**: Default 144 blocks (~24 hours)
- **Max Locations per Item**: 20
- **Max Items per Location**: 50

## Error Codes

- `u401`: Unauthorized operation
- `u402`: Invalid price value
- `u403`: Invalid location
- `u404`: Invalid item
- `u405`: Already voted
- `u406`: Price not found
- `u407`: Insufficient stake
- `u408`: Cooldown period active

## Development

### Testing
```bash
clarinet test
```

### Deployment
```bash
clarinet deploy
```

### Check Contract
```bash
clarinet check
```

## License

MIT License
