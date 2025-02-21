{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Types where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3
import Plutarch.Prelude
import Plutarch.Repr.Data
import Plutarch.Repr.SOP
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx

-- | This type is the Haskell-level Pool config, it mirrors the PPoolConfig fields.
data PoolConfig = PoolConfig
    { validitySymbol :: CurrencySymbol
    , stakingRewardsSymbol :: CurrencySymbol
    , enforcedScriptOutputDatumHash :: DatumHash
    , treasuryHolderScriptHash :: ScriptHash
    }
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''PoolConfig
PlutusTx.makeIsDataSchemaIndexed ''PoolConfig [('PoolConfig, 0)]

{- |
The compile-time configuration for the pool contract.
-}
data PPoolConfig (s :: S) = PPoolConfig
    { validitySymbol :: Term s (PAsData PCurrencySymbol)
    -- ^ Shares use that as their currency symbol
    , stakingRewardsSymbol :: Term s (PAsData PCurrencySymbol)
    -- ^ A token with this currency symbol authorizes the addition of staking rewards
    , enforcedScriptOutputDatumHash :: Term s (PAsData PDatumHash)
    -- ^ This datum hash is used for the compensation outputs with script addresses
    , treasuryHolderScriptHash :: Term s (PAsData PScriptHash)
    -- ^ This script hash is used when extracting fees to treasury
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData)
    deriving (PlutusType) via (DeriveAsDataStruct PPoolConfig)

deriving via DeriveDataPLiftable PPoolConfig PoolConfig instance PLiftable PPoolConfig

-- | This type is the Haskell-level Pool redeemer, it mirrors the PPoolRedeemer fields.
data PoolRedeemer = Evolve [PlutusTx.Integer]
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''PoolRedeemer [('Evolve, 0)]

{- |
This redeemer is passed to the Pool validator and contains the indices of request inputs.
-}
newtype PPoolRedeemer (s :: S) = PEvolve {requestsIndices :: Term s (PAsData (PBuiltinList (PAsData PInteger)))}
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData)
    deriving (PlutusType) via (DeriveAsDataStruct PPoolRedeemer)

-- | This type is the Haskell-level Pool datum, it mirrors the PPoolDatum fields.
data PoolDatum = PoolDatum
    { pdRequestHash :: ScriptHash
    , pdASymbol :: CurrencySymbol
    , pdAToken :: TokenName
    , pdBSymbol :: CurrencySymbol
    , pdBToken :: TokenName
    }
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

{- |
This is pool datum, it doesn't change.
-}
data PPoolDatum (s :: S) = PPoolDatum
    { requestHash :: Term s (PAsData PScriptHash)
    -- ^ Stored script hash of the request validator
    -- ^ It's stored to remove the circular dependency between scripts
    -- ^ With this the request can just depend on the pool hash directly.
    , aSymbol :: Term s (PAsData PCurrencySymbol)
    -- ^ The currency symbol of the token A, may be ada
    , aToken :: Term s (PAsData PTokenName)
    -- ^ The asset name of the token A, may be ada
    , bSymbol :: Term s (PAsData PCurrencySymbol)
    -- ^ The currency symbol of the token B, may not be ada
    , bToken :: Term s (PAsData PTokenName)
    -- ^ The asset name of the token B, may not be ada
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData)
    deriving (PlutusType) via (DeriveAsDataStruct PPoolDatum)

data PFlatAssets (s :: S) = PFlatAssets
    { fltA :: Term s PInteger
    , fltB :: Term s PInteger
    , fltShares :: Term s PInteger
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic)
    deriving (PlutusType) via (DeriveAsSOPStruct PFlatAssets)

-- | This type is the Haskell-level Request config, it mirrors the PRequestConfig fields.
data RequestConfig = RequestConfig
    { pool :: ScriptHash
    }
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''RequestConfig
PlutusTx.makeIsDataSchemaIndexed ''RequestConfig [('RequestConfig, 0)]

{- |
The compile-time configuration for the pool contract.
-}
newtype PRequestConfig (s :: S) = PRequestConfig
    { pool :: Term s (PAsData PScriptHash)
    -- ^ Pool script hash
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData)
    deriving (PlutusType) via (DeriveAsDataStruct PRequestConfig)

deriving via DeriveDataPLiftable PRequestConfig RequestConfig instance PLiftable PRequestConfig

-- | This type is a Haskell-level type that mirrors PSwapDirection
data SwapDirection
    = SwapAToB
    | SwapBToA
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

data PSwapDirection (s :: S)
    = -- | User has asset A and wants asset B
      PSwapAToB
    | -- | User has asset B and wants asset A
      PSwapBToA
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData)
    deriving (PlutusType) via (DeriveAsDataStruct PSwapDirection)

-- | This type is a Haskell-level type that mirrors PRequestAction
data RequestAction
    = Swap SwapDirection PlutusTx.Integer
    | AddLiquidity PlutusTx.Integer
    | WithdrawLiquidity PlutusTx.Integer PlutusTx.Integer
    | ExtractTreasury
    | AddStakingRewards
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

data PRequestAction (s :: S)
    = -- | User wants to swap the locked asset, receiving at least a given quantity of wanted asset
      PSwap
        { swapDirection :: Term s (PAsData PSwapDirection)
        , minWantedTokens :: Term s (PAsData PInteger)
        }
    | -- | User wants to add liquidity to the pool, receiving minimum number of pool shares in return
      PAddLiquidity
        {minWantedShares :: Term s (PAsData PInteger)}
    | -- | User wants to withdraw liquidity from the pool.
      PWithdrawLiquidity
        { minWantedA :: Term s (PAsData PInteger)
        , minWantedB :: Term s (PAsData PInteger)
        }
    | -- | User wants to trigger withdrawing the assets that belong to the treasury from the pool.
      PExtractTreasury
    | -- | Staking agent adds the staking rewards back into the pool
      PAddStakingRewards
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData)
    deriving (PlutusType) via (DeriveAsDataStruct PRequestAction)

-- | This type is the Haskell-level Request datum, it mirrors the PRequestDatum fields.
data RequestDatum = RequestDatum
    { rdBeneficiary :: Address
    , rdOwnerAddress :: Address
    , rdDeadline :: POSIXTime
    , rdAssetASymbol :: CurrencySymbol
    , rdAssetAToken :: TokenName
    , rdAssetBSymbol :: CurrencySymbol
    , rdAssetBToken :: TokenName
    , rdAction :: RequestAction
    }
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

data PRequestDatum (s :: S)
    = PRequestDatum
    { beneficiary :: Term s (PAsData PAddress)
    -- ^ This address receives the compensations, might be a script address
    , ownerAddress :: Term s (PAsData PAddress)
    -- ^ This address can revoke the request, can't be a script address;
    -- ^ it's a full address instead of the pub key so it's easier to create a revoke tx
    , deadline :: Term s (PAsData PPosixTime)
    -- ^ The request is valid until this deadline
    , assetASymbol :: Term s (PAsData PCurrencySymbol)
    -- ^ The request targets this token A
    , assetAToken :: Term s (PAsData PTokenName)
    -- ^ The request targets this token A
    , assetBSymbol :: Term s (PAsData PCurrencySymbol)
    -- ^ The request targets this token B
    , assetBToken :: Term s (PAsData PTokenName)
    -- ^ The request targets this token B
    , action :: Term s (PAsData PRequestAction)
    -- ^ The request action
    -- ^ one of swap / add liquidity / withdraw liquidity / extract treasury / add staking rewards
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData)
    deriving (PlutusType) via (DeriveAsDataStruct PRequestDatum)

-- | This type is the Haskell-level Request redeemer, it mirrors the PRequestRedeemer fields.
data RequestRedeemer
    = Apply PlutusTx.Integer
    | Reclaim
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''RequestRedeemer [('Apply, 0), ('Reclaim, 1)]

data PRequestRedeemer (s :: S)
    = -- | The request is applied against the pool at the specified index
      PApply {poolIndex :: Term s (PAsData PInteger)}
    | -- | The request is revoked by the owner
      PReclaim
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData)
    deriving (PlutusType) via (DeriveAsDataStruct PRequestRedeemer)

-- | This type is used to contain the relevant parsed request information
data PParsedRequest (s :: S) = PParsedRequest
    { requestAction :: Term s PRequestAction
    , requestAssets :: Term s PFlatAssets
    , compensationAssets :: Term s PFlatAssets
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic)
    deriving (PlutusType) via (DeriveAsSOPStruct PParsedRequest)
