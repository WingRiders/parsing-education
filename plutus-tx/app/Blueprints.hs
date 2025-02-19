{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import qualified Data.ByteString.Short as Short
import qualified Data.Set as Set
import PlutusLedgerApi.Common (serialiseCompiledCode, toBuiltin)
import PlutusLedgerApi.Data.V3 (DatumHash (..), ScriptHash (..))
import qualified PlutusLedgerApi.V1.Value as Value
import PlutusTx.Blueprint
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import Pool
import Request
import Types

poolConfig :: PoolConfig
poolConfig =
    PoolConfig
        { pcEnforcedScriptOutputDatumHash =
            DatumHash
                ( stringToBuiltinByteStringHex
                    "00000000000000000000000000000000000000000000000000000000"
                )
        , pcStakingRewardsSymbol =
            -- TODO: staking
            Value.CurrencySymbol
                ( stringToBuiltinByteStringHex
                    "00000000000000000000000000000000000000000000000000000000"
                )
        , pcTreasuryHolderScriptHash =
            -- TODO: treasury
            ScriptHash
                ( stringToBuiltinByteStringHex
                    "0000000000000000000000000000000000000000\
                    \0000000000000000000000000000000000000000"
                )
        , pcValiditySymbol =
            -- TODO: validity
            Value.CurrencySymbol
                ( stringToBuiltinByteStringHex
                    "00000000000000000000000000000000000000000000000000000000"
                )
        }

poolValidatorCompiled :: CompiledValidator
poolValidatorCompiled =
    compiledValidator PlutusV3
        . Short.fromShort
        . serialiseCompiledCode
        . poolValidatorScript
        $ poolConfig

poolValidator :: ValidatorBlueprint referencedTypes
poolValidator =
    MkValidatorBlueprint
        { validatorTitle = "Pool Validator"
        , validatorDescription =
            Just "Plutus script validating pool transactions"
        , validatorParameters =
            [ MkParameterBlueprint
                { parameterTitle = Just "Parameters"
                , parameterDescription = Just "Compile-time validator parameters"
                , parameterPurpose = Set.singleton Spend
                , parameterSchema = definitionRef @PoolConfig
                }
            ]
        , validatorRedeemer =
            MkArgumentBlueprint
                { argumentTitle = Just "Redeemer"
                , argumentDescription = Just "Redeemer for the pool validator"
                , argumentPurpose = Set.singleton Spend
                , argumentSchema = definitionRef @PoolRedeemer
                }
        , validatorDatum =
            Just $
                MkArgumentBlueprint
                    { argumentTitle = Just "Datum"
                    , argumentDescription = Just "Datum for the pool validator"
                    , argumentPurpose = Set.singleton Spend
                    , argumentSchema = definitionRef @PoolDatum
                    }
        , validatorCompiled = Just poolValidatorCompiled
        }

requestConfig :: RequestConfig
requestConfig =
    RequestConfig{rpPool = ScriptHash . toBuiltin . compiledValidatorHash $ poolValidatorCompiled}

requestValidatorCompiled :: CompiledValidator
requestValidatorCompiled =
    compiledValidator PlutusV3
        . Short.fromShort
        . serialiseCompiledCode
        . requestValidatorScript
        $ requestConfig

requestValidator :: ValidatorBlueprint referencedTypes
requestValidator =
    MkValidatorBlueprint
        { validatorTitle = "Request Validator"
        , validatorDescription =
            Just "Plutus script validating request transactions"
        , validatorParameters =
            [ MkParameterBlueprint
                { parameterTitle = Just "Parameters"
                , parameterDescription = Just "Compile-time validator parameters"
                , parameterPurpose = Set.singleton Spend
                , parameterSchema = definitionRef @RequestConfig
                }
            ]
        , validatorRedeemer =
            MkArgumentBlueprint
                { argumentTitle = Just "Redeemer"
                , argumentDescription = Just "Redeemer for the request validator"
                , argumentPurpose = Set.singleton Spend
                , argumentSchema = definitionRef @RequestRedeemer
                }
        , validatorDatum =
            Just $
                MkArgumentBlueprint
                    { argumentTitle = Just "Datum"
                    , argumentDescription = Just "Datum for the request validator"
                    , argumentPurpose = Set.singleton Spend
                    , argumentSchema = definitionRef @RequestDatum
                    }
        , validatorCompiled = do
            let script = requestValidatorScript requestConfig
            let code = Short.fromShort (serialiseCompiledCode script)
            Just (compiledValidator PlutusV3 code)
        }

preamble :: Preamble
preamble =
    MkPreamble
        { preambleTitle = "Parsing Education"
        , preambleDescription =
            Just "Blueprint for a set of Plutus scripts to illustrate parsing"
        , preambleVersion = "1.1.0"
        , preamblePlutusVersion = PlutusV3
        , preambleLicense = Just "MIT"
        }

contractBlueprint :: ContractBlueprint
contractBlueprint =
    MkContractBlueprint
        { contractId = Just "parsing-education"
        , contractPreamble = preamble
        , contractValidators = Set.fromList [poolValidator, requestValidator]
        , contractDefinitions = deriveDefinitions @[PoolConfig, PoolRedeemer, RequestConfig, RequestRedeemer]
        }

main :: IO ()
main = writeBlueprint "artifacts/plutus.json" contractBlueprint
