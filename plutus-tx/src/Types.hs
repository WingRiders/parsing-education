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

module Types where

import GHC.Generics (Generic)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

{- |
The compile-time configuration for the pool contract.
-}
data PoolConfig = PoolConfig
    { pcValiditySymbol :: CurrencySymbol
    -- ^ Shares use that as their currency symbol
    , pcStakingRewardsSymbol :: CurrencySymbol
    -- ^ A token with this currency symbol authorizes the addition of staking rewards
    , pcEnforcedScriptOutputDatumHash :: DatumHash
    -- ^ This datum hash is used for the compensation outputs with script addresses
    , pcTreasuryHolderScriptHash :: ScriptHash
    -- ^ This script hash is used when extracting fees to treasury
    }
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''PoolConfig
PlutusTx.makeIsDataSchemaIndexed ''PoolConfig [('PoolConfig, 0)]

{- |
This redeemer is passed to the Pool validator and contains the indices of request inputs.
-}
data PoolRedeemer = Evolve [Integer]
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed
    ''PoolRedeemer
    [('Evolve, 0)]

{- |
This is pool datum, it doesn't change.
-}
data PoolDatum = PoolDatum
    { pdRequestHash :: ScriptHash
    -- ^ Stored script hash of the request validator
    -- ^ It's stored to remove the circular dependency between scripts
    -- ^ With this the request can just depend on the pool hash directly.
    , pdASymbol :: CurrencySymbol
    -- ^ The currency symbol of the token A, may be ada
    , pdAToken :: TokenName
    -- ^ The asset name of the token A, may be ada
    , pdBSymbol :: CurrencySymbol
    -- ^ The currency symbol of the token B, may not be ada
    , pdBToken :: TokenName
    -- ^ The asset name of the token B, may not be ada
    }
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataIndexed ''PoolDatum [('PoolDatum, 0)]

{- |
 Plutus `Value` is very inefficient to use.
 We therefore introduce our own helper which keeps important asset values in a compact and efficient form.

 Note: FlatAssets represent Value *without* any oilAda
-}
data FlatAssets = FlatAssets
    { fltA :: Integer
    -- ^ amount of token A
    , fltB :: Integer
    -- ^ amount of token B
    , fltShares :: Integer
    -- ^ amount of pool shares
    }
    deriving (Prelude.Eq, Prelude.Show)

data RequestConfig = RequestConfig
    { rpPool :: ScriptHash
    -- ^ Pool script hash
    }
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''RequestConfig
PlutusTx.makeIsDataSchemaIndexed ''RequestConfig [('RequestConfig, 0)]

data SwapDirection
    = -- | User has asset A and wants asset B
      SwapAToB
    | -- | User has asset B and wants asset A
      SwapBToA
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''SwapDirection [('SwapAToB, 0), ('SwapBToA, 1)]

data RequestAction
    = -- | User wants to swap the locked asset, receiving at least a given quantity of wanted asset
      Swap SwapDirection Integer
    | -- | User wants to add liquidity to the pool, receiving minimum number of pool shares in return
      AddLiquidity Integer
    | -- | User wants to withdraw liquidity from the pool.
      WithdrawLiquidity Integer Integer
    | -- | User wants to trigger withdrawing the assets that belong to the treasury from the pool.
      ExtractTreasury
    | -- | Staking agent adds the staking rewards back into the pool
      AddStakingRewards
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed
    ''RequestAction
    [ ('Swap, 0)
    , ('AddLiquidity, 1)
    , ('WithdrawLiquidity, 2)
    , ('ExtractTreasury, 3)
    , ('AddStakingRewards, 4)
    ]

data RequestDatum = RequestDatum
    { rdBeneficiary :: Address
    -- ^ This address receives the compensations, might be a script address
    , rdOwnerAddress :: Address
    -- ^ This address can revoke the request, can't be a script address;
    -- ^ it's a full address instead of the pub key so it's easier to create a revoke tx
    , rdDeadline :: POSIXTime
    -- ^ The request is valid until this deadline
    , rdAssetASymbol :: CurrencySymbol
    -- ^ The request targets this token A
    , rdAssetAToken :: TokenName
    -- ^ The request targets this token A
    , rdAssetBSymbol :: CurrencySymbol
    -- ^ The request targets this token B
    , rdAssetBToken :: TokenName
    -- ^ The request targets this token B
    , rdAction :: RequestAction
    -- ^ The request action
    -- ^ one of swap / add liquidity / withdraw liquidity / extract treasury / add staking rewards
    }
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''RequestDatum [('RequestDatum, 0)]

data RequestRedeemer
    = -- | The request is applied against the pool at the specified index
      Apply Integer
    | -- | The request is revoked by the owner
      Reclaim
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''RequestRedeemer [('Apply, 0), ('Reclaim, 1)]

-- | This type is used to contain the relevant parsed request information
data ParsedRequest = ParsedRequest
    { requestAction :: RequestAction
    , requestAssets :: FlatAssets
    , compensationAssets :: FlatAssets
    }

{- |
This datum is used for compensations on script addresses.
Without it the agent can set the datum hash to a random hash and make the output unspendable.
-}
data EnforcedScriptOutDatum = EnforcedScriptOutDatum
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''EnforcedScriptOutDatum
PlutusTx.makeIsDataSchemaIndexed ''EnforcedScriptOutDatum [('EnforcedScriptOutDatum, 123)]
