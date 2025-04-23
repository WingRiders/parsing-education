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

module Request where

import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts (txSignedBy)
import PlutusTx
import PlutusTx.Prelude
import Types
import Util

-- | Reclaims are allowed if the request is signed by the request owner
{-# INLINEABLE validateReclaim #-}
validateReclaim :: RequestDatum -> ScriptContext -> Bool
validateReclaim datum (ScriptContext txInfo ~_redeemer ~_datum) = signedByOwner
  where
    signedByOwner = traceIfFalse "signedByOwner" (txSignedBy txInfo ownerHash)
    ownerHash = addressPubKeyCredential (rdOwnerAddress datum)

-- | Applications are allowed if there's a pool input on the specified location
{-# INLINEABLE validateApply #-}
validateApply :: RequestConfig -> ScriptContext -> Integer -> Bool
validateApply requestConfig context poolIndex = poolIsCorrect
  where
    poolIsCorrect = traceIfFalse "poolIsCorrect" (delegateeValidatorHash == rpPool requestConfig)
    delegateeValidatorHash = extractNthInputValidatorHash context poolIndex

{-# INLINEABLE requestTypedValidator #-}
requestTypedValidator ::
    RequestConfig ->
    RequestDatum ->
    RequestRedeemer ->
    ScriptContext ->
    Bool
requestTypedValidator requestConfig datum redeemer context =
    case redeemer of
        Apply poolIndex -> validateApply requestConfig context poolIndex
        Reclaim -> validateReclaim datum context

{-# INLINEABLE requestUntypedValidator #-}
requestUntypedValidator ::
    RequestConfig ->
    BuiltinData ->
    BuiltinData ->
    BuiltinUnit
requestUntypedValidator requestConfig _ ctx =
    check
        ( case unsafeFromBuiltinData ctx of
            context@(ScriptContext ~_txInfo (Redeemer redeemer) (SpendingScript ~_ (Just (Datum datum)))) ->
                requestTypedValidator
                    requestConfig
                    (unsafeFromBuiltinData datum)
                    (unsafeFromBuiltinData redeemer)
                    context
            _else -> traceError "nodatum"
        )

requestValidatorScript ::
    RequestConfig ->
    CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
requestValidatorScript requestConfig =
    $$(compile [||requestUntypedValidator||]) `unsafeApplyCode` liftCodeDef requestConfig
