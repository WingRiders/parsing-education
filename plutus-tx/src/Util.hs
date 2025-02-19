{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UndecidableInstances #-}
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

module Util (
    addressPubKeyCredential,
    extractNthInputValidatorHash,
    containsDuplicate,
    finiteTxValidityRangeTimestamps,
    zip',
    flatAssetsFromValue,
    scriptHashFromAddress,
    isScriptAddress,
    fromMaybe',
    shareClassToken,
) where

import PlutusLedgerApi.V1 (valueOf)
import PlutusLedgerApi.V3
import PlutusTx.Prelude
import Types

{- |
 Extracts FlatAssets from value (minus oilAda).

 Warning: this ignores any other tokens in the value!
-}
{-# INLINEABLE flatAssetsFromValue #-}
flatAssetsFromValue ::
    (CurrencySymbol, TokenName) ->
    (CurrencySymbol, TokenName) ->
    (CurrencySymbol, TokenName) ->
    Value ->
    Integer ->
    FlatAssets
flatAssetsFromValue (assetACs, assetATn) (assetBCs, assetBTn) (shareCs, shareTn) val oilAda =
    -- Here we assume that `val` is non-negative (which is the case for TxOut value) as well as oilAda >=0
    FlatAssets qtyA qtyB qtyShares
  where
    qtyA = valueOfWithOilCheck val assetACs assetATn oilAda
    qtyB = valueOf val assetBCs assetBTn
    qtyShares = valueOf val shareCs shareTn

{- |
 Get the quantity of the given asset in the 'Value' while checking
 that there is enough oil in the value. If asset is ada, returned quantity is without oil.

 Note: This function throws if oil ada does not check up.
-}
{-# INLINEABLE valueOfWithOilCheck #-}
valueOfWithOilCheck :: Value -> CurrencySymbol -> TokenName -> Integer -> Integer
valueOfWithOilCheck val policyId name oilAda
    -- Here we assume that `val` is non-negative (which is the case for TxOut value) as well as oilAda >=0
    | if assetIsAda then qtyAdaWithoutOil >= 0 else qtyAdaWithoutOil == 0 = qty
    | otherwise = traceError "oil"
  where
    assetIsAda = policyId == adaSymbol
    qtyAdaWithoutOil = valueOf val adaSymbol adaToken - oilAda
    qty = if assetIsAda then qtyAdaWithoutOil else valueOf val policyId name

{-# INLINEABLE scriptHashFromAddress #-}
scriptHashFromAddress :: Address -> ScriptHash
scriptHashFromAddress addr = case addr of
    Address (ScriptCredential vHash) _stakingCredential -> vHash
    _else -> traceError "hash"

{-# INLINEABLE isScriptAddress #-}
isScriptAddress :: Address -> Bool
isScriptAddress addr = case addressCredential addr of
    ScriptCredential _ -> True
    PubKeyCredential _ -> False

{- |
 Turns 'Just x' into 'x', throwing if `None`
-}
{-# INLINEABLE fromMaybe' #-}
fromMaybe' :: Maybe a -> a
fromMaybe' a = case a of
    Just x -> x
    Nothing -> traceError "maybe"

{- |
 Alternative to zip, throwing error when there is not enough elements in `b`.
 Used in pool validator, when zipping requests with compensation tx outputs. There needs to be
 a tx output for every request.
-}
{-# INLINEABLE zip' #-}
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _bs = [] -- second list may have leftovers. This is OK
zip' _as [] = traceError "zip"
zip' (a : as) (b : bs) = (a, b) : zip' as bs

{-# INLINEABLE containsDuplicate #-}
containsDuplicate :: [Integer] -> Bool
containsDuplicate l = containsDuplicate' l []
  where
    containsDuplicate' [] _ = False
    containsDuplicate' (y : ys) xs
        | y `elem` xs = True
        | otherwise = containsDuplicate' ys (y : xs)

{-# INLINEABLE finiteTxValidityRangeTimestamps #-}
finiteTxValidityRangeTimestamps :: POSIXTimeRange -> (POSIXTime, POSIXTime)
finiteTxValidityRangeTimestamps r = case r of
    (Interval (LowerBound (Finite startTimestamp) _) (UpperBound (Finite endTimestamp) _)) ->
        (startTimestamp, endTimestamp)
    _don't_care -> traceError "finiterange"

{-# INLINEABLE addressPubKeyCredential #-}
addressPubKeyCredential :: Address -> PubKeyHash
addressPubKeyCredential address = case addressCredential address of
    PubKeyCredential h -> h
    ScriptCredential _ -> traceError "pubkey"

{-# INLINEABLE extractNthInput #-}
extractNthInput :: ScriptContext -> Integer -> TxInInfo
extractNthInput (ScriptContext txInfo _ _) i =
    txInfoInputs txInfo !! i

{-# INLINEABLE extractNthInputValidatorHash #-}
extractNthInputValidatorHash :: ScriptContext -> Integer -> ScriptHash
extractNthInputValidatorHash context i = hash
  where
    hash = case extractNthInput context i of
        TxInInfo _ (TxOut (Address (ScriptCredential h) _) _ _ _) -> h
        _don't_care -> traceError "nth"

{- |
Share class token is obtained with the following formula:

sha3_256(sha3_256(token a) <> sha3_256(token b))

This gives a unique token name per each token pair.
-}
{-# INLINEABLE shareClassToken #-}
shareClassToken :: (CurrencySymbol, TokenName) -> (CurrencySymbol, TokenName) -> TokenName
shareClassToken (aSymbol, aToken) (bSymbol, bToken) =
    TokenName . sha3_256 $ assetClassHash aSymbol aToken <> assetClassHash bSymbol bToken
  where
    assetClassHash cs tn = sha3_256 $ unCurrencySymbol cs <> unTokenName tn
