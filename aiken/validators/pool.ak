use aiken/builtin.{tail_list}
use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/crypto.{DataHash, ScriptHash}
use cardano/assets.{AssetName, PolicyId, Value, ada_policy_id, quantity_of}
use cardano/transaction.{
  DatumHash, InlineDatum, Input, NoDatum, Output, OutputReference, Transaction,
}
use constants.{agent_fee_ada, request_oil_ada}
use types.{
  AddStakingRewards, Evolve, ExtractTreasury, ParsedRequest, PoolConfig,
  PoolDatum, PoolRedeemer, RequestDatum,
}
use utils.{
  contains_duplicate, finite_tx_validity_range_timestamps, is_script_address,
  script_hash_from_address, share_class_asset_name, zip,
}

validator pool(config: PoolConfig) {
  spend(
    datum: Option<Data>,
    redeemer: Data,
    _ref: OutputReference,
    transaction: Transaction,
  ) {
    trace @"Pool run"
    expect Some(datum_some) = datum
    expect pool_datum: PoolDatum = datum_some
    expect pool_redeemer: PoolRedeemer = redeemer
    let Evolve { request_indices } = pool_redeemer
    validate_pool_evolve(config, pool_datum, request_indices, transaction)?
  }

  else(_) {
    fail
  }
}

/// Pool evolve transactions include one pool input and 1+ request inputs.
/// The pool does the bulk of the validation.
/// It scans over the requests, parses them and makes sure each is compensated.
fn validate_pool_evolve(
  config: PoolConfig,
  datum: PoolDatum,
  request_indices: List<Int>,
  transaction: Transaction,
) {
  let share_policy_id = config.validity_policy_id
  let share_asset_name =
    share_class_asset_name(
      datum.a_policy_id,
      datum.a_asset_name,
      datum.b_policy_id,
      datum.b_asset_name,
    )

  let (_, tx_validity_end) =
    finite_tx_validity_range_timestamps(transaction.validity_range)

  let no_request_duplicated = !contains_duplicate(request_indices)

  let ordered_inputs =
    list.map(
      request_indices,
      fn(i) {
        expect Some(input) = list.at(transaction.inputs, i)
        input
      },
    )
  // the first output is the pool, the rest are compensations
  let ordered_outputs = tail_list(transaction.outputs)
  let parsed_requests =
    parse_requests_and_validate_metadata(
      config,
      datum.a_policy_id,
      datum.a_asset_name,
      datum.b_policy_id,
      datum.b_asset_name,
      share_policy_id,
      share_asset_name,
      datum.request_hash,
      tx_validity_end,
      transaction.datums,
      ordered_inputs,
      ordered_outputs,
    )

  and {
    // No request index can be duplicated by the agent, otherwise funds can be stolen
    no_request_duplicated?,
    // There is at least one processed request
    (list.length(parsed_requests) > 0)?,
  }
}

fn parse_requests_and_validate_metadata(
  config: PoolConfig,
  a_policy_id: PolicyId,
  a_asset_name: AssetName,
  b_policy_id: PolicyId,
  b_asset_name: AssetName,
  share_policy_id: PolicyId,
  share_asset_name: AssetName,
  request_hash: ScriptHash,
  tx_validity_end: Int,
  datums: Dict<DataHash, Data>,
  ordered_inputs: List<Input>,
  ordered_outputs: List<Output>,
) -> List<ParsedRequest> {
  list.map(
    zip(ordered_inputs, ordered_outputs),
    fn((input, output)) {
      let request_datum = unsafe_request_datum(input.output, datums)
      let r_assets =
        utils.flat_assets_from_value(
          a_policy_id,
          a_asset_name,
          b_policy_id,
          b_asset_name,
          share_policy_id,
          share_asset_name,
          input.output.value,
          request_oil_ada + agent_fee_ada,
        )
      let c_assets =
        utils.flat_assets_from_value(
          a_policy_id,
          a_asset_name,
          b_policy_id,
          b_asset_name,
          share_policy_id,
          share_asset_name,
          output.value,
          request_oil_ada,
        )

      let actual_validator_hash = script_hash_from_address(input.output.address)

      let expected_out_datum_hash =
        if is_script_address(request_datum.beneficiary) {
          // The fixed output datum hash for script compensations
          DatumHash(config.enforced_script_output_datum_hash)
        } else {
          // And no output datum for pub key compensations
          NoDatum
        }

      let action_is_allowed =
        when request_datum.action is {
          ExtractTreasury ->
            // Treasury must be extracted to a treasury script
            script_hash_from_address(request_datum.beneficiary) == config.treasury_holder_script_hash
          AddStakingRewards ->
            // Staking rewards are additionally authorized
            is_authorized_to_add_staking_rewards(
              config,
              a_policy_id,
              share_asset_name,
              input.output.value,
              output.value,
            )
          _ -> True
        }

      if and {
        // The output address must be the request beneficiary
        (output.address == request_datum.beneficiary)?,
        // The request application must happen before the deadline
        (tx_validity_end < request_datum.deadline)?,
        // The input actually has the request hash
        (actual_validator_hash == request_hash)?,
        // The token A policy id matches
        (request_datum.a_policy_id == a_policy_id)?,
        // The token A asset name matches
        (request_datum.a_asset_name == a_asset_name)?,
        // The token B policy id matches
        (request_datum.b_policy_id == b_policy_id)?,
        // The token B asset name matches
        (request_datum.b_asset_name == b_asset_name)?,
        // the compensation has the expected out datum
        (output.datum == expected_out_datum_hash)?,
        // the action is actually allowed
        action_is_allowed?,
      } {
        ParsedRequest {
          request_action: request_datum.action,
          request_assets: r_assets,
          compensation_assets: c_assets,
        }
      } else {
        fail
      }
    },
  )
}

/// Checks whether the add staking rewards request is authorized
///  1. The pool has to be an ADA <> token pool
///  2. The request contains a staking rewards token with the tokenName matching the LP assets
///  3. The reward token was returned as a compensation
fn is_authorized_to_add_staking_rewards(
  config: PoolConfig,
  a_policy_id: PolicyId,
  share_asset_name: AssetName,
  request_value: Value,
  compensation_value: Value,
) {
  let reward_policy_id = config.staking_rewards_policy_id
  let pool_id = share_asset_name
  let has_reward_token =
    quantity_of(request_value, reward_policy_id, pool_id) == 1
  let did_return_reward_token =
    quantity_of(compensation_value, reward_policy_id, pool_id) == 1

  and {
    ada_policy_id == a_policy_id,
    has_reward_token,
    did_return_reward_token,
  }
}

fn unsafe_request_datum(
  output: Output,
  datums: Dict<DataHash, Data>,
) -> RequestDatum {
  // Both inline and hash are supported
  let d =
    when output.datum is {
      InlineDatum(datum) -> datum
      DatumHash(hash) -> {
        expect Some(d) = dict.get(datums, hash)
        d
      }
      NoDatum -> fail
    }
  expect datum: RequestDatum = d
  datum
}
