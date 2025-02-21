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

import Data.ByteString.Short qualified as Short
import Data.Set qualified as Set
import Plutarch.Internal.Term (Config (..), LogLevel (..), TracingMode (..), compile)
import Plutarch.Prelude
import Plutarch.Script
import PlutusLedgerApi.Common (toBuiltin)
import PlutusLedgerApi.Data.V3 (DatumHash (..), ScriptHash (..))
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusTx.Blueprint
import Pool
import Request
import Types

c :: ClosedTerm a -> Script
c t = case compile (Tracing LogInfo DoTracing) t of
    Right r -> r
    Left _ -> error "failed to compile"

poolConfig :: PoolConfig
poolConfig =
    PoolConfig
        { enforcedScriptOutputDatumHash =
            DatumHash "00000000000000000000000000000000000000000000000000000000"
        , stakingRewardsSymbol =
            Value.CurrencySymbol "00000000000000000000000000000000000000000000000000000000"
        , treasuryHolderScriptHash =
            ScriptHash
                "0000000000000000000000000000000000000000\
                \0000000000000000000000000000000000000000"
        , validitySymbol =
            Value.CurrencySymbol "00000000000000000000000000000000000000000000000000000000"
        }

poolValidatorCompiled :: CompiledValidator
poolValidatorCompiled =
    compiledValidator PlutusV3
        . Short.fromShort
        . serialiseScript
        $ c (poolValidator # pconstant poolConfig)

poolValidatorB :: ValidatorBlueprint referencedTypes
poolValidatorB =
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
    RequestConfig{pool = ScriptHash . toBuiltin . compiledValidatorHash $ poolValidatorCompiled}

requestValidatorCompiled :: CompiledValidator
requestValidatorCompiled =
    compiledValidator PlutusV3
        . Short.fromShort
        . serialiseScript
        $ c (requestValidator # pconstant requestConfig)

requestValidatorB :: ValidatorBlueprint referencedTypes
requestValidatorB =
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
            let script = c $ requestValidator # pconstant requestConfig
            let code = Short.fromShort (serialiseScript script)
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
        , contractValidators = Set.fromList [poolValidatorB, requestValidatorB]
        , contractDefinitions = deriveDefinitions @[PoolConfig, PoolRedeemer, RequestConfig, RequestRedeemer]
        }

main :: IO ()
main = writeBlueprint "artifacts/plutus.json" contractBlueprint
