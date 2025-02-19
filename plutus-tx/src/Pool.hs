{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Pool where

import Constants
import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude
import PlutusTx.Prelude qualified as PlutusTx
import Types
import Util

{- |
Pool evolve transactions include one pool input and 1+ request inputs.
The pool does the bulk of the validation.
It scans over the requests, parses them and makes sure each is compensated.
-}
{-# INLINEABLE validatePoolEvolve #-}
validatePoolEvolve :: PoolConfig -> PoolDatum -> [Integer] -> ScriptContext -> Bool
validatePoolEvolve poolConfig datum requestLocations (ScriptContext txInfo _ _) =
    -- No request index can be duplicated by the agent, otherwise funds can be stolen
    traceIfFalse "noRequestDuplicated" noRequestDuplicated
        &&
        -- There is at least one processed request
        traceIfFalse "requestsCount" (requestsCount > 0)
  where
    requestsCount = length parsedRequests
    (_, txValidityEnd) = finiteTxValidityRangeTimestamps (txInfoValidRange txInfo)

    inputs = txInfoInputs txInfo
    outputs = txInfoOutputs txInfo

    datums = txInfoData txInfo

    noRequestDuplicated = not $ containsDuplicate requestLocations

    aSymbol = pdASymbol datum
    aToken = pdAToken datum
    bSymbol = pdBSymbol datum
    bToken = pdBToken datum
    shareSymbol = pcValiditySymbol poolConfig
    shareToken = shareClassToken (aSymbol, aToken) (bSymbol, bToken)

    parsedRequests =
        [ parseRequestAndValidateMetadata
            poolConfig
            (aSymbol, aToken)
            (bSymbol, bToken)
            (shareSymbol, shareToken)
            (pdRequestHash datum)
            txValidityEnd
            input
            d
            output
        | (input', output) <- zip' orderedInputs orderedOutputs
        , let input = txInInfoResolved input'
              d = unsafeRequestDatum input datums
        ]
      where
        orderedInputs = [inputs !! i | i <- requestLocations]
        -- the first output is the pool, the rest are compensations
        orderedOutputs = tail outputs

{-# INLINEABLE parseRequestAndValidateMetadata #-}
parseRequestAndValidateMetadata :: PoolConfig -> (CurrencySymbol, TokenName) -> (CurrencySymbol, TokenName) -> (CurrencySymbol, TokenName) -> ScriptHash -> POSIXTime -> TxOut -> RequestDatum -> TxOut -> ParsedRequest
parseRequestAndValidateMetadata poolConfig (assetACs, assetATn) (assetBCs, assetBTn) (shareCs, shareTn) expectedRequestValidatorHash txValidityRangeEnd input datum output =
    if -- The output address must be the request beneficiary
    traceIfFalse "out address" (txOutAddress output == rdBeneficiary datum)
        && traceIfFalse "range" (txValidityRangeEnd <= rdDeadline datum) -- The request application must happen before the deadline
        && traceIfFalse "request hash" (actualValidatorHash == expectedRequestValidatorHash) -- The input actually has the request hash
        && traceIfFalse "asymbol" (rdAssetASymbol datum == assetACs) -- The token A currency symbol matches
        && traceIfFalse "atoken" (rdAssetAToken datum == assetATn) -- The token A asset name matches
        && traceIfFalse "bsymbol" (rdAssetBSymbol datum == assetBCs) -- The token B currency symbol matches
        && traceIfFalse "btoken" (rdAssetBToken datum == assetBTn) -- The token B asset name matches
        && traceIfFalse "outdatum" (txOutDatum output == expectedOutDatumHash) -- the compensation has the expected out datum
        && traceIfFalse "allowed" actionIsAllowed -- the action is actually allowed
        then ParsedRequest (rdAction datum) rAssets cAssets
        else traceError "parse"
  where
    ~rAssets = flatAssetsFromValue (assetACs, assetATn) (assetBCs, assetBTn) (shareCs, shareTn) requestValue (requestOilAda + agentFeeAda)
    ~cAssets = flatAssetsFromValue (assetACs, assetATn) (assetBCs, assetBTn) (shareCs, shareTn) compensationValue requestOilAda

    actualValidatorHash = scriptHashFromAddress $ txOutAddress input

    expectedOutDatumHash =
        if isScriptAddress (rdBeneficiary datum)
            -- The fixed output datum hash for script compensations
            then OutputDatumHash (pcEnforcedScriptOutputDatumHash poolConfig)
            -- And no output datum for pub key compensations
            else NoOutputDatum

    ~requestValue = txOutValue input

    ~compensationValue = txOutValue output

    actionIsAllowed = case rdAction datum of
        ExtractTreasury ->
            -- Treasury must be extracted to a treasury script
            scriptHashFromAddress (rdBeneficiary datum) == pcTreasuryHolderScriptHash poolConfig
        AddStakingRewards ->
            -- Staking rewards are additionally authorized
            isAuthorizedToAddStakingRewards poolConfig assetACs shareTn requestValue compensationValue
        _else -> True

{-# INLINEABLE unsafeRequestDatum #-}
unsafeRequestDatum :: TxOut -> Map DatumHash Datum -> RequestDatum
unsafeRequestDatum txOut txDatums = datum
  where
    datum = PlutusTx.unsafeFromBuiltinData d
    -- Both inline and hash are supported
    Datum d = case txOutDatum txOut of
        OutputDatum dat -> dat
        OutputDatumHash hash -> fromMaybe' $ Map.lookup hash txDatums
        NoOutputDatum -> traceError "nooutputdatum"

{- |
 Checks whether the add staking rewards request is authorized
  1. The pool has to be an ADA <> token pool
  2. The request contains a staking rewards token with the tokenName matching the LP assets
  3. The reward token was returned as a compensation
-}
{-# INLINEABLE isAuthorizedToAddStakingRewards #-}
isAuthorizedToAddStakingRewards :: PoolConfig -> CurrencySymbol -> TokenName -> Value -> Value -> Bool
isAuthorizedToAddStakingRewards poolConfig aSymbol shareToken requestValue compensationValue =
    traceIfFalse "ada" (adaSymbol == aSymbol)
        && traceIfFalse "reward token" hasRewardToken
        && traceIfFalse "return reward token" didReturnRewardToken
  where
    rewardSymbol = pcStakingRewardsSymbol poolConfig
    -- Pool id == share token name == rewards token name
    poolId = shareToken
    hasRewardToken = valueOf requestValue rewardSymbol poolId == 1
    didReturnRewardToken = valueOf compensationValue rewardSymbol poolId == 1

{-# INLINEABLE poolTypedValidator #-}
poolTypedValidator :: PoolConfig -> PoolDatum -> PoolRedeemer -> ScriptContext -> Bool
poolTypedValidator poolConfig datum redeemer ctx = case redeemer of
    Evolve requestLocations -> validatePoolEvolve poolConfig datum requestLocations ctx

{-# INLINEABLE poolUntypedValidator #-}
poolUntypedValidator :: PoolConfig -> BuiltinData -> BuiltinUnit
poolUntypedValidator poolConfig ctx =
    PlutusTx.check
        ( case PlutusTx.unsafeFromBuiltinData ctx of
            context@(ScriptContext ~_txInfo (Redeemer redeemer) (SpendingScript ~_ (Just (Datum datum)))) ->
                poolTypedValidator
                    poolConfig
                    (PlutusTx.unsafeFromBuiltinData datum)
                    (PlutusTx.unsafeFromBuiltinData redeemer)
                    context
            _else -> traceError "nodatum"
        )

poolValidatorScript ::
    PoolConfig ->
    CompiledCode (BuiltinData -> BuiltinUnit)
poolValidatorScript poolConfig =
    $$(compile [||poolUntypedValidator||]) `unsafeApplyCode` liftCodeDef poolConfig
