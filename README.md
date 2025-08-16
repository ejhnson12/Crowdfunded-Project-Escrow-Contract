# 🚀 Crowdfunded Project Escrow Contract

A Clarity smart contract that enables secure crowdfunding with built-in escrow functionality on the Stacks blockchain.

## 📋 Overview

This contract allows creators to launch crowdfunding campaigns with automatic fund management:
- 💰 **Secure Escrow**: Funds are held in the contract until goals are met
- 🎯 **Goal-Based Release**: Creators can only claim funds if funding goals are reached
- 🔄 **Automatic Refunds**: Backers get refunded if goals aren't met by deadline
- ⏰ **Time-Based Control**: Deadlines ensure campaigns don't run indefinitely

## 🔧 Core Features

### For Project Creators 👨‍💼
- Create projects with funding goals and deadlines
- Claim funds when goals are reached after deadline
- Cancel projects with zero contributions

### For Backers 🤝
- Contribute STX to projects before deadline
- Automatic refund eligibility if goals aren't met
- Track contributions across multiple projects

## 📝 Contract Functions

### Public Functions

#### `create-project`
```clarity
(create-project title description funding-goal deadline)
```
Creates a new crowdfunding project.
- **title**: Project name (max 100 chars)
- **description**: Project details (max 500 chars) 
- **funding-goal**: Target amount in microSTX
- **deadline**: Block height when funding ends

#### `contribute`
```clarity
(contribute project-id amount)
```
Contribute STX to a project.
- **project-id**: ID of the project to fund
- **amount**: Amount in microSTX to contribute

#### `claim-funds`
```clarity
(claim-funds project-id)
```
Claim funds for a successful project (creator only).
- **project-id**: ID of the project to claim funds from

#### `refund`
```clarity
(refund project-id)
```
Get refund from a failed project (contributors only).
- **project-id**: ID of the project to get refund from

#### `cancel-project`
```clarity
(cancel-project project-id)
```
Cancel a project with zero contributions (creator only).
- **project-id**: ID of the project to cancel

### Read-Only Functions

#### `get-project`
```clarity
(get-project project-id)
```
Returns complete project information.

#### `get-contribution`
```clarity
(get-contribution project-id contributor)
```
Returns contributor's total contribution to a project.

#### `is-goal-reached`
```clarity
(is-goal-reached project-id)
```
Checks if project has reached its funding goal.

#### `can-claim-funds`
```clarity
(can-claim-funds project-id)
```
Checks if creator can claim funds for a project.

## 🚀 Usage Examples

### 1. Create a Project
```bash
clarinet console
```
```clarity
(contract-call? .crowdfunded-project-escrow-contract create-project 
  "My Awesome App" 
  "Building the next big thing in DeFi" 
  u1000000000 
  u1000)
```

### 2. Contribute to a Project
```clarity
(contract-call? .crowdfunded-project-escrow-contract contribute u1 u100000000)
```

### 3. Check Project Status
```clarity
(contract-call? .crowdfunded-project-escrow-contract get-project u1)
```

### 4. Claim Funds (after goal reached and deadline passed)
```clarity
(contract-call? .crowdfunded-project-escrow-contract claim-funds u1)
```

### 5. Get Refund (if goal not reached after deadline)
```clarity
(contract-call? .crowdfunded-project-escrow-contract refund u1)
```

## 🔒 Security Features

- ✅ **Time-locked**: Contributors can only contribute before deadline
- ✅ **Goal-protected**: Funds only released when goals are met
- ✅ **Creator-verified**: Only project creators can claim their funds
- ✅ **Refund-guaranteed**: Automatic refund eligibility for failed projects
- ✅ **Double-spend protection**: Prevents multiple claims/refunds

## 🧪 Testing

Run the test suite:
```bash
clarinet test
```

Check contract syntax:
```bash
clarinet check
```

## 📊 Project States

| State | Description | Available Actions |
|-------|-------------|-------------------|
| **Active** | Project accepting contributions | contribute |
| **Successful** | Goal reached, deadline passed | claim-funds |
| **Failed** | Goal not reached, deadline passed | refund |
| **Claimed** | Funds claimed by creator | None |
| **Cancelled** | Cancelled by creator | None |

## 💡 Error Codes

| Code | Description |
|------|-------------|
| u100 | Owner only |
| u101 | Not found |
| u102 | Unauthorized |
| u103 | Invalid amount |
| u104 | Goal not reached |
| u105 | Deadline passed |
| u106 | Deadline not passed |
| u107 | Already claimed |
| u108 | Already refunded |
| u109 | Invalid deadline |
| u110 | Project not active |
| u111 | Insufficient funds |

## 🔗 Integration

This contract can be integrated with:
- 🌐 Web frontends via Stacks.js
- 📱 Mobile apps using Stacks Connect
- 🤖 Other smart contracts through contract calls
- 📊 Analytics dashboards for tracking

## 📄 License

MIT License - feel free to use and modify for your projects!

---

Built with ❤️ on Stacks blockchain
