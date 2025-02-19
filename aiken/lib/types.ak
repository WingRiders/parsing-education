use aiken/crypto.{DataHash, ScriptHash}
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId}

/// The compile-time configuration for the pool contract.
pub type PoolConfig {
  /// Shares use that as their currency symbol
  validity_policy_id: PolicyId,
  /// A token with this policy id authorizes the addition of staking rewards
  staking_rewards_policy_id: PolicyId,
  /// This datum hash is used for the compensation outputs with script addresses
  enforced_script_output_datum_hash: DataHash,
  /// This script hash is used when extracting fees to treasury
  treasury_holder_script_hash: ScriptHash,
}

// This redeemer is passed to the Pool validator and contains the indices of request inputs.
pub type PoolRedeemer {
  Evolve { request_indices: List<Int> }
}

// This is pool datum, it doesn't change.
pub type PoolDatum {
  /// Stored script hash of the request validator
  /// It's stored to remove the circular dependency between scripts
  /// With this the request can just depend on the pool hash directly.
  request_hash: ScriptHash,
  /// The currency symbol of the token A, may be ada
  a_policy_id: PolicyId,
  /// The asset name of the token A, may be ada
  a_asset_name: AssetName,
  /// The currency symbol of the token B, may not be ada
  b_policy_id: PolicyId,
  /// The asset name of the token B, may not be ada
  b_asset_name: AssetName,
}

pub type FlatAssets {
  flt_a: Int,
  flt_b: Int,
  flt_shares: Int,
}

/// The compile-time configuration for the pool contract.
pub type RequestConfig {
  /// Pool script hash
  pool_hash: ScriptHash,
}

pub type SwapDirection {
  /// User has asset A and wants asset B
  SwapAToB
  /// User has asset B and wants asset A
  SwapBToA
}

pub type RequestAction {
  /// User wants to swap the locked asset, receiving at least a given quantity of wanted asset
  Swap { direction: SwapDirection, min_wanted_tokens: Int }
  /// User wants to add liquidity to the pool, receiving minimum number of pool shares in return
  AddLiquidity { min_wanted_shares: Int }
  /// User wants to withdraw liquidity from the pool.
  WithdrawLiquidity { min_wanted_a: Int, min_wanted_b: Int }
  /// User wants to trigger withdrawing the assets that belong to the treasury from the pool.
  ExtractTreasury
  /// Staking agent adds the staking rewards back into the pool
  AddStakingRewards
}

pub type RequestDatum {
  /// This address receives the compensations, might be a script address
  beneficiary: Address,
  /// This address can revoke the request, can't be a script address;
  /// it's a full address instead of the pub key so it's easier to create a revoke tx
  owner_address: Address,
  /// The request is valid until this deadline
  deadline: Int,
  /// The request targets this token A
  a_policy_id: PolicyId,
  /// The request targets this token A
  a_asset_name: AssetName,
  /// The request targets this token B
  b_policy_id: PolicyId,
  /// The request targets this token B
  b_asset_name: AssetName,
  // The request action
  // one of swap / add liquidity / withdraw liquidity / extract treasury / add staking rewards
  action: RequestAction,
}

pub type RequestRedeemer {
  /// The request is applied against the pool at the specified index
  Apply { pool_index: Int }
  /// The request is revoked by the owner
  Reclaim
}

/// This type is used to contain the relevant parsed request information
pub type ParsedRequest {
  request_action: RequestAction,
  request_assets: FlatAssets,
  compensation_assets: FlatAssets,
}
