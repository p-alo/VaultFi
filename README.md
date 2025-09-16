# VaultFi

## Overview

VaultFi is a decentralized lending protocol that enables users to lend and borrow assets with dynamic interest rates determined by utilization. It includes mechanisms for collateral management, liquidation, and interest accrual to ensure stability and risk mitigation.

## Features

* **Lending Pools**: Create and manage asset-specific pools.
* **Deposits and Withdrawals**: Supply assets and redeem deposits with collateral toggle options.
* **Borrowing**: Borrow against supplied collateral while maintaining a safe health factor.
* **Repayment**: Repay loans partially or fully with interest adjustment.
* **Liquidation**: Liquidators can repay unhealthy borrower positions in exchange for collateral.
* **Interest Accrual**: Rates adjust dynamically based on utilization.
* **Oracles**: Price feeds for asset valuation.
* **Health Factor**: Determines user solvency and liquidation eligibility.

## Data Structures

* **available-resources**: Tracks pool configuration and utilization.
* **pool-tokens**: Represents shares in lending pools.
* **account-deposits**: User deposits and collateral settings.
* **account-loans**: User loan balances with interest tracking.
* **rate-model-data**: Interest rate model per asset.
* **pricing-oracles**: Oracle configuration and cached prices.

## Key Functions

* **initialize-pool**: Set up a new asset lending pool.
* **deposit-resource**: Supply tokens into a pool and receive pool tokens.
* **withdraw-resource**: Redeem underlying assets by burning pool tokens.
* **toggle-backing**: Enable or disable deposits as collateral.
* **borrow-resource**: Borrow assets using collateralized deposits.
* **repay-loan**: Repay borrowed assets with accrued interest.
* **liquidate-position**: Liquidate unhealthy borrower positions.
* **calculate-interest**: Accrue interest for a pool.
* **get-wellness-factor**: Read-only health factor for a user.

## Error Codes

* **u1000–u1018**: Authorization, invalid parameters, insufficient balances, collateral violations, and liquidation constraints.

## Configuration

* **system-administrator**: Protocol administrator.
* **system-fee-recipient**: Address receiving protocol fees.
* **liquidation-penalty-percentage**: Penalty applied during liquidation.
* **system-fee-percentage**: Protocol fee percentage.
* **minimum-health-factor**: Threshold below which positions can be liquidated.
* **liquidation-boundary**: Collateral factor applied to health factor.

## Usage Flow

1. **Administrator** initializes asset pools with parameters.
2. **Users** deposit assets, receive pool tokens, and optionally enable collateral.
3. **Users** borrow assets if collateralized sufficiently.
4. **Borrowers** repay loans or risk liquidation if health factor falls below the minimum.
5. **Liquidators** repay unhealthy loans and seize discounted collateral.
6. **Protocol** accrues interest and distributes system fees.
