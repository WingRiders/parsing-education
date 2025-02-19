use cardano/transaction.{OutputReference, Transaction}
use types.{Apply, Reclaim, RequestConfig, RequestDatum, RequestRedeemer}
use utils

/// Reclaims are allowed if the request is signed by the request owner
fn validate_reclaim(datum: RequestDatum, transaction: Transaction) {
  let owner_hash = utils.address_pub_key_credential(datum.owner_address)
  let signed_by_owner =
    utils.tx_signed_by(transaction.extra_signatories, owner_hash)
  signed_by_owner
}

/// Applications are allowed if there's a pool input on the specified location
fn validate_apply(
  request_config: RequestConfig,
  transaction: Transaction,
  pool_index: Int,
) {
  let delegatee_validator_hash =
    utils.extract_nth_input_validator_hash(transaction, pool_index)
  let pool_is_correct = delegatee_validator_hash == request_config.pool_hash

  pool_is_correct
}

validator request(request_config: RequestConfig) {
  spend(
    datum: Option<Data>,
    redeemer: Data,
    _ref: OutputReference,
    transaction: Transaction,
  ) {
    expect Some(datum_some) = datum
    expect request_datum: RequestDatum = datum_some
    expect request_redeemer: RequestRedeemer = redeemer
    when request_redeemer is {
      Apply { pool_index } ->
        validate_apply(request_config, transaction, pool_index)?
      Reclaim -> validate_reclaim(request_datum, transaction)?
    }
  }

  else(_) {
    fail
  }
}
