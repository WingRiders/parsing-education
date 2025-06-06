module Constants where

{- |
 This is the minimum amount of ada that needs to be added in requests. It will not be used and will be fully returned
-}
{-# INLINEABLE requestOilAda #-}
requestOilAda :: (Num a) => a
requestOilAda = 2000000

{- |
 This is a fee that needs to be included in every request. It compensates the agents for the tx fee.
-}
{-# INLINEABLE agentFeeAda #-}
agentFeeAda :: (Num a) => a
agentFeeAda = 2000000
