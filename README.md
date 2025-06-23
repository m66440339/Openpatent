# 🔬 Openpatent - Decentralized Patent Management DAO

A blockchain-based patent management system that enables open intellectual property registration, licensing, and community governance through a decentralized autonomous organization (DAO).

## 🌟 Features

- **📝 Patent Registration**: Register intellectual property with customizable usage fees and duration
- **💰 Licensing System**: Pay-per-use licensing with automatic fee distribution
- **🗳️ DAO Governance**: Community-driven decision making through staked voting
- **🔒 Stake-based Voting**: Secure governance through token staking mechanisms
- **⏰ Time-locked Patents**: Automatic patent expiration and renewal system
- **📊 Usage Tracking**: Comprehensive analytics for patent usage and revenue

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone the repository
2. Navigate to the project directory
3. Run Clarinet console:

```bash
clarinet console
```

## 📖 Usage Guide

### 🔬 Registering a Patent

Register your intellectual property on-chain:

```clarity
(contract-call? .Openpatent register-patent 
  "AI Algorithm" 
  "Machine learning algorithm for predictive analytics" 
  u5000000 
  u52560)
```

Parameters:
- `title`: Patent title (max 100 chars)
- `description`: Detailed description (max 500 chars)  
- `usage-fee`: Fee in microSTX for each license
- `duration`: Patent duration in blocks

### 💳 Licensing a Patent

Purchase a license to use patented technology:

```clarity
(contract-call? .Openpatent license-patent u1)
```

### 🏛️ DAO Participation

#### Stake Tokens
Stake STX to participate in governance:

```clarity
(contract-call? .Openpatent stake-tokens u10000000)
```

#### Create Proposals
Submit governance proposals:

```clarity
(contract-call? .Openpatent create-proposal 
  u1 
  "deactivate" 
  "Patent violates community guidelines")
```

#### Vote on Proposals
Cast your vote on active proposals:

```clarity
(contract-call? .Openpatent vote-on-proposal u1 true)
```

#### Execute Proposals
Execute passed proposals after voting period:

```clarity
(contract-call? .Openpatent execute-proposal u1)
```

### 💸 Withdraw Stakes

Withdraw your staked tokens after lock period:

```clarity
(contract-call? .Openpatent withdraw-stake)
```

## 🔍 Read-Only Functions

Query contract state without transactions:

- `get-patent`: Retrieve patent details
- `get-patent-usage`: Check user's patent usage history
- `get-proposal`: View proposal information
- `get-user-stake`: Check staking balance
- `is-patent-active`: Verify patent status

## ⚙️ Configuration

### Constants

- **MIN_PATENT_DURATION**: 144 blocks (~24 hours)
- **MAX_PATENT_DURATION**: 52,560 blocks (~1 year)
- **MIN_USAGE_FEE**: 1,000,000 microSTX (1 STX)
- **MIN_STAKE_AMOUNT**: 10,000,000 microSTX (10 STX)
- **VOTING_PERIOD**: 1,008 blocks (~1 week)
