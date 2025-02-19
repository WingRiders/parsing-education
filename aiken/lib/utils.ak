use aiken/collection/list
use aiken/crypto.{ScriptHash, VerificationKeyHash, sha3_256}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{
  AssetName, PolicyId, Value, ada_policy_id, lovelace_of, quantity_of,
}
use cardano/transaction.{Input, Output, Transaction, ValidityRange}
use types.{FlatAssets}

/// Extracts PFlatAssets from value (minus oilAda).
///
/// Warning: this ignores any other tokens in the value!
pub fn flat_assets_from_value(
  a_policy_id: PolicyId,
  a_asset_name: AssetName,
  b_policy_id: PolicyId,
  b_asset_name: AssetName,
  share_policy_id: PolicyId,
  share_asset_name: AssetName,
  value: Value,
  oil_ada: Int,
) -> FlatAssets {
  let qty_a =
    quantity_of_with_oil_check(value, a_policy_id, a_asset_name, oil_ada)
  let qty_b = quantity_of(value, b_policy_id, b_asset_name)
  let qty_shares = quantity_of(value, share_policy_id, share_asset_name)
  FlatAssets { flt_a: qty_a, flt_b: qty_b, flt_shares: qty_shares }
}

/// Get the quantity of the given asset in the 'PValue' while checking
/// that there is enough oil in the value. If asset is ada, returned quantity is without oil.
///
/// Note: This function throws if oil ada does not check up.
fn quantity_of_with_oil_check(
  value: Value,
  policy_id: PolicyId,
  asset_name: AssetName,
  oil_ada: Int,
) {
  let asset_is_ada = policy_id == ada_policy_id

  let qty_ada_without_oil = lovelace_of(value) - oil_ada

  let check =
    if asset_is_ada {
      qty_ada_without_oil >= 0
    } else {
      qty_ada_without_oil == 0
    }
  let qty =
    if asset_is_ada {
      qty_ada_without_oil
    } else {
      quantity_of(value, policy_id, asset_name)
    }
  if check {
    qty
  } else {
    fail
  }
}

pub fn script_hash_from_address(address: Address) -> ScriptHash {
  expect Script(hash) = address.payment_credential
  hash
}

pub fn is_script_address(address: Address) {
  when address.payment_credential is {
    Script(_) -> True
    _ -> False
  }
}

/// Alternative to zip, throwing error when there is not enough elements in `b`.
/// Used in pool validator, when zipping requests with compensation tx outputs. There needs to be
/// a tx output for every request.
pub fn zip(as_: List<a>, bs_: List<b>) -> List<(a, b)> {
  when as_ is {
    [x, ..xs] ->
      when bs_ is {
        [y, ..ys] -> [(x, y), ..zip(xs, ys)]
        // Each element from the first list must have a corresponding element
        [] -> fail
      }
    // We allow leftovers in the second list
    [] -> []
  }
}

pub fn address_pub_key_credential(address: Address) -> VerificationKeyHash {
  expect VerificationKey(hash) = address.payment_credential
  hash
}

pub fn tx_signed_by(
  signatories: List<VerificationKeyHash>,
  hash: VerificationKeyHash,
) {
  list.has(signatories, hash)
}

fn extract_nth_input(transaction: Transaction, index: Int) -> Input {
  expect Some(input) = list.at(transaction.inputs, index)
  input
}

pub fn extract_nth_input_validator_hash(
  transaction: Transaction,
  index: Int,
) -> ScriptHash {
  let input = extract_nth_input(transaction, index)
  expect Script(hash) = input.output.address.payment_credential
  hash
}

pub fn contains_duplicate(l: List<Int>) {
  contains_duplicate_impl(l, [])
}

fn contains_duplicate_impl(l: List<Int>, acc: List<Int>) {
  when l is {
    [index, ..indices] ->
      if list.has(acc, index) {
        True
      } else {
        contains_duplicate_impl(indices, [index, ..acc])
      }
    [] -> False
  }
}

pub fn finite_tx_validity_range_timestamps(r: ValidityRange) -> (Int, Int) {
  let Interval { lower_bound, upper_bound } = r
  let start_time =
    when lower_bound.bound_type is {
      Finite(t) -> t
      _ -> fail
    }
  let end_time =
    when upper_bound.bound_type is {
      Finite(t) -> t
      _ -> fail
    }
  (start_time, end_time)
}

/// Share class token is obtained with the following formula:
///
/// sha3_256(sha3_256(token a) <> sha3_256(token b))
///
/// This gives a unique token name per each token pair.
pub fn share_class_asset_name(
  a_policy_id: PolicyId,
  a_asset_name: AssetName,
  b_policy_id: PolicyId,
  b_asset_name: AssetName,
) -> AssetName {
  let asset_class_hash =
    fn(policy_id, asset_name) {
      bytearray.concat(sha3_256(policy_id), sha3_256(asset_name))
    }
  sha3_256(
    bytearray.concat(
      asset_class_hash(a_policy_id, a_asset_name),
      asset_class_hash(b_policy_id, b_asset_name),
    ),
  )
}
