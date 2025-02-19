{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (SomeException, evaluate, try)
import qualified PlutusLedgerApi.V1.Value as Value
import PlutusLedgerApi.V3
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Test.Tasty
import Test.Tasty.HUnit
import Types (FlatAssets (..))
import qualified Util

throws :: a -> IO ()
throws (thunk :: a) = do
    result <- try (evaluate thunk) :: IO (Either SomeException a)
    case result of
        Left _ -> pure ()
        Right _ -> assertFailure "Expected an exception but got a result"

tests :: TestTree
tests =
    testGroup
        "Contracts unit tests"
        [ -- containsDuplicate
          testCase "containsDuplicate []" $
            Util.containsDuplicate [] @?= False
        , testCase "containsDuplicate [1]" $
            Util.containsDuplicate [1] @?= False
        , testCase "containsDuplicate [1, 2]" $
            Util.containsDuplicate [1, 2] @?= False
        , testCase "containsDuplicate [1, 1]" $
            Util.containsDuplicate [1, 1] @?= True
        , testCase "containsDuplicate [1, 2, 1]" $
            Util.containsDuplicate [1, 2, 1] @?= True
        , -- addressPubKeyCredential
          testCase "addressPubKeyCredential on a pub key address" $
            Util.addressPubKeyCredential (Address (PubKeyCredential "deadbeef") Nothing) @?= "deadbeef"
        , testCase "addressPubKeyCredential on a script address" . throws $
            Util.addressPubKeyCredential (Address (ScriptCredential "deadbeef") Nothing)
        , -- scriptHashFromAddress
          testCase "scriptHashFromAddress on a pub key address" . throws $
            Util.scriptHashFromAddress (Address (PubKeyCredential "deadbeef") Nothing)
        , testCase "scriptHashFromAddress on a script address" $
            Util.scriptHashFromAddress (Address (ScriptCredential "deadbeef") Nothing) @?= "deadbeef"
        , -- finiteTxValidityRangeTimestamps
          testCase "finiteTxValidityRangeTimestamps on finite including bounds" $
            Util.finiteTxValidityRangeTimestamps (Interval (LowerBound (Finite 0) True) (UpperBound (Finite 1) True))
                @?= (0, 1)
        , testCase "finiteTxValidityRangeTimestamps on finite excluding bounds" $
            Util.finiteTxValidityRangeTimestamps (Interval (LowerBound (Finite 0) False) (UpperBound (Finite 1) False))
                @?= (0, 1)
        , testCase "finiteTxValidityRangeTimestamps on lower -inf" . throws $
            Util.finiteTxValidityRangeTimestamps (Interval (LowerBound NegInf False) (UpperBound (Finite 1) False))
        , testCase "finiteTxValidityRangeTimestamps on lower +inf" . throws $
            Util.finiteTxValidityRangeTimestamps (Interval (LowerBound PosInf False) (UpperBound (Finite 1) False))
        , testCase "finiteTxValidityRangeTimestamps on upper -inf" . throws $
            Util.finiteTxValidityRangeTimestamps (Interval (LowerBound (Finite 0) False) (UpperBound NegInf False))
        , testCase "finiteTxValidityRangeTimestamps on upper +inf" . throws $
            Util.finiteTxValidityRangeTimestamps (Interval (LowerBound (Finite 0) False) (UpperBound PosInf False))
        , -- isScriptAddress
          testCase "isScriptAddress on a pub key address" $
            Util.isScriptAddress (Address (PubKeyCredential "deadbeef") Nothing) @?= False
        , testCase "isScriptAddress on a script address" $
            Util.isScriptAddress (Address (ScriptCredential "deadbeef") Nothing) @?= True
        , -- fromMaybe'
          testCase "fromMaybe' on a Just value" $
            Util.fromMaybe' (Just True) @?= True
        , testCase "fromMaybe' on a Nothing" . throws $
            Util.fromMaybe' Nothing
        , -- zip'
          testCase "zip' on [] []" $
            Util.zip' [] [] @?= ([] :: [(Int, Int)])
        , testCase "zip' on [1] []" . throws $
            Util.zip' [1 :: Int] []
        , testCase "zip' on [] [1]" $
            Util.zip' [] [1] @?= ([] :: [(Int, Int)])
        , testCase "zip' on [1] [2]" $
            Util.zip' [1] [2] @?= ([(1, 2)] :: [(Int, Int)])
        , testCase "zip' on [1, 2] [3, 4]" $
            Util.zip' [1, 2] [3, 4] @?= ([(1, 3), (2, 4)] :: [(Int, Int)])
        , -- flatAssetsFromValue
          testCase "flatAssetsFromValue with a != ada, enough oil" $
            Util.flatAssetsFromValue
                (CurrencySymbol "a", TokenName "a")
                (CurrencySymbol "b", TokenName "b")
                (CurrencySymbol "h", TokenName "h")
                ( Value.singleton (CurrencySymbol "a") (TokenName "a") 1
                    <> Value.singleton (CurrencySymbol "b") (TokenName "b") 2
                    <> Value.singleton (CurrencySymbol "h") (TokenName "h") 3
                    <> Value.singleton (CurrencySymbol "") (TokenName "") 2000000
                )
                2000000
                @?= FlatAssets 1 2 3
        , testCase "flatAssetsFromValue with a != ada, not enough oil" . throws $
            Util.flatAssetsFromValue
                (CurrencySymbol "a", TokenName "a")
                (CurrencySymbol "b", TokenName "b")
                (CurrencySymbol "h", TokenName "h")
                ( Value.singleton (CurrencySymbol "a") (TokenName "a") 1
                    <> Value.singleton (CurrencySymbol "b") (TokenName "b") 2
                    <> Value.singleton (CurrencySymbol "h") (TokenName "h") 3
                    <> Value.singleton (CurrencySymbol "") (TokenName "") (2000000 - 1)
                )
                2000000
        , testCase "flatAssetsFromValue with a == ada, enough oil" $
            Util.flatAssetsFromValue
                (CurrencySymbol "", TokenName "")
                (CurrencySymbol "b", TokenName "b")
                (CurrencySymbol "h", TokenName "h")
                ( Value.singleton (CurrencySymbol "b") (TokenName "b") 2
                    <> Value.singleton (CurrencySymbol "h") (TokenName "h") 3
                    <> Value.singleton (CurrencySymbol "") (TokenName "") (5 + 2000000)
                )
                2000000
                @?= FlatAssets 5 2 3
        , testCase "flatAssetsFromValue with a == ada, not enough oil" . throws $
            Util.flatAssetsFromValue
                (CurrencySymbol "", TokenName "")
                (CurrencySymbol "b", TokenName "b")
                (CurrencySymbol "h", TokenName "h")
                ( Value.singleton (CurrencySymbol "b") (TokenName "b") 2
                    <> Value.singleton (CurrencySymbol "h") (TokenName "h") 3
                    <> Value.singleton (CurrencySymbol "") (TokenName "") (2000000 - 1)
                )
                2000000
        , -- shareClassToken
          testCase "shareClassToken with an ada pool" $
            Util.shareClassToken
                (CurrencySymbol "", TokenName "")
                (CurrencySymbol "b", TokenName "b")
                @?= TokenName "\171\171eO\241\181\248%q@\EOTA\ETXc\SOH\ESC\225e\RS\168s\146\192\149\206\145\238\175\140\&9\"U"
        , testCase "shareClassToken with a non-ada pool" $
            Util.shareClassToken
                (CurrencySymbol "a", TokenName "a")
                (CurrencySymbol "b", TokenName "b")
                @?= TokenName "\170\191C\202\154\142\214\130\USY\191\NAK\153 :\187\147A\232;,sAOJ\170\135\181`\138=\251"
        ]

-- TODO: tests
main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    defaultMain tests
