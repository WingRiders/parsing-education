use aiken/interval.{
  Finite, Interval, IntervalBound, NegativeInfinity, PositiveInfinity,
}
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets
use types.{FlatAssets}
use utils

test contains_duplicate_empty() {
  utils.contains_duplicate([]) == False
}

test contains_duplicate_1() {
  utils.contains_duplicate([1]) == False
}

test contains_duplicate_1_2() {
  utils.contains_duplicate([1, 2]) == False
}

test contains_duplicate_1_1() {
  utils.contains_duplicate([1, 1]) == True
}

test contains_duplicate_1_2_1() {
  utils.contains_duplicate([1, 2, 1]) == True
}

test address_pub_key_credential_on_a_pub_key_address() {
  utils.address_pub_key_credential(Address(VerificationKey("deadbeef"), None)) == "deadbeef"
}

test address_pub_key_credential_on_a_script_address() fail {
  expect _ = utils.address_pub_key_credential(Address(Script("deadbeef"), None))
}

test script_hash_from_address_on_a_pub_key_address() fail {
  expect _ =
    utils.script_hash_from_address(Address(VerificationKey("deadbeef"), None))
}

test script_hash_from_address_on_a_script_address() {
  utils.script_hash_from_address(Address(Script("deadbeef"), None)) == "deadbeef"
}

test finite_tx_validity_range_timestamps_finite_including() {
  utils.finite_tx_validity_range_timestamps(
    Interval(IntervalBound(Finite(0), True), IntervalBound(Finite(1), True)),
  ) == (0, 1)
}

test finite_tx_validity_range_timestamps_finite_excluding() {
  utils.finite_tx_validity_range_timestamps(
    Interval(IntervalBound(Finite(0), False), IntervalBound(Finite(1), False)),
  ) == (0, 1)
}

test finite_tx_validity_range_timestamps_lower_neg_inf() fail {
  expect _ =
    utils.finite_tx_validity_range_timestamps(
      Interval(
        IntervalBound(NegativeInfinity, False),
        IntervalBound(Finite(1), False),
      ),
    )
}

test finite_tx_validity_range_timestamps_lower_pos_inf() fail {
  expect _ =
    utils.finite_tx_validity_range_timestamps(
      Interval(
        IntervalBound(PositiveInfinity, False),
        IntervalBound(Finite(1), False),
      ),
    )
}

test finite_tx_validity_range_timestamps_upper_neg_inf() fail {
  expect _ =
    utils.finite_tx_validity_range_timestamps(
      Interval(
        IntervalBound(Finite(0), False),
        IntervalBound(NegativeInfinity, False),
      ),
    )
}

test finite_tx_validity_range_timestamps_upper_pos_inf() fail {
  expect _ =
    utils.finite_tx_validity_range_timestamps(
      Interval(
        IntervalBound(Finite(0), False),
        IntervalBound(PositiveInfinity, False),
      ),
    )
}

test zip_empty__empty() {
  utils.zip([], []) == []
}

test zip_1__empty() fail {
  expect _ = utils.zip([1], [])
}

test zip_1__2() {
  utils.zip([1], [2]) == [(1, 2)]
}

test zip_1_2__3_4() {
  utils.zip([1, 2], [3, 4]) == [(1, 3), (2, 4)]
}

test flat_assets_from_value_with_a_non_ada_enough_oil() {
  utils.flat_assets_from_value(
    "a",
    "a",
    "b",
    "b",
    "h",
    "h",
    assets.from_asset_list(
      [
        Pair("a", [Pair("a", 1)]), Pair("b", [Pair("b", 2)]),
        Pair("h", [Pair("h", 3)]), Pair("", [Pair("", 2_000_000)]),
      ],
    ),
    2_000_000,
  ) == FlatAssets(1, 2, 3)
}

test flat_assets_from_value_with_a_non_ada_not_enough_oil() fail {
  expect _ =
    utils.flat_assets_from_value(
      "a",
      "a",
      "b",
      "b",
      "h",
      "h",
      assets.from_asset_list(
        [
          Pair("a", [Pair("a", 1)]),
          Pair("b", [Pair("b", 2)]),
          Pair("h", [Pair("h", 3)]),
          Pair("", [Pair("", 2_000_000 - 1)]),
        ],
      ),
      2_000_000,
    )
}

test flat_assets_from_value_with_a_ada_enough_oil() {
  utils.flat_assets_from_value(
    "",
    "",
    "b",
    "b",
    "h",
    "h",
    assets.from_asset_list(
      [
        Pair("b", [Pair("b", 2)]),
        Pair("h", [Pair("h", 3)]),
        Pair("", [Pair("", 2_000_000 + 1)]),
      ],
    ),
    2_000_000,
  ) == FlatAssets(1, 2, 3)
}

test flat_assets_from_value_with_a_ada_not_enough_oil() fail {
  expect _ =
    utils.flat_assets_from_value(
      "",
      "",
      "b",
      "b",
      "h",
      "h",
      assets.from_asset_list(
        [
          Pair("b", [Pair("b", 2)]),
          Pair("h", [Pair("h", 3)]),
          Pair("", [Pair("", 2_000_000 - 1)]),
        ],
      ),
      2_000_000,
    )
}

test share_class_asset_name_with_an_ada_pool() {
  utils.share_class_asset_name("", "", "b", "b") == #"9ddfa9618bc7dcb038d3cadada183f7aec7cfc7365fb94ba557f3a4a05d7a2e9"
}

test share_class_asset_name_with_a_non_ada_pool() {
  utils.share_class_asset_name("a", "a", "b", "b") == #"ea29e07dc5c9eeb36e8c01c096196a98b6e06754a4184a3ebec1806a9a141088"
}
