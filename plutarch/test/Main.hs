module Main where

import GHC.IO.Handle (hSetEncoding)
import Plutarch.Builtin.Bool (pfalse, ptrue)
import Plutarch.Internal.ListLike (pconvertLists)
import Plutarch.LedgerApi.V3
import Plutarch.Prelude
import Plutarch.Test
import PlutusLedgerApi.V3
import System.IO (stderr, stdout, utf8)
import Test.Tasty
import Test.Tasty.HUnit
import Types (PFlatAssets (..))
import Util qualified

pconstantMapData ::
    ( PElemConstraint list a
    , PElemConstraint list (PAsData a)
    , PListLike list
    , PIsData a
    , PLiftable (list a)
    ) =>
    AsHaskell (list a) ->
    Term s (list (PAsData a))
pconstantMapData x = pmap # plam pdata # pconstant x

asPairs :: Term s (PBuiltinList (PBuiltinPair PInteger PInteger) :--> PList (PPair PInteger PInteger))
asPairs = Util.pmapToPList # plam (\p -> pcon $ PPair (pfstBuiltin # p) (psndBuiltin # p))

tests :: TestTree
tests =
    testGroup
        "Contracts unit test"
        [ -- pcontainsDuplicate
          testCase "pcontainsDuplicate []" $
            Util.pcontainsDuplicate (pconstantMapData []) #@?= pfalse
        , testCase "pcontainsDuplicate [1]" $
            Util.pcontainsDuplicate (pconstantMapData [1]) #@?= pfalse
        , testCase "pcontainsDuplicate [1, 2]" $
            Util.pcontainsDuplicate (pconstantMapData [1, 2]) #@?= pfalse
        , testCase "pcontainsDuplicate [1, 1]" $
            Util.pcontainsDuplicate (pconstantMapData [1, 1]) #@?= ptrue
        , testCase "pcontainsDuplicate [1, 2, 1]" $
            Util.pcontainsDuplicate (pconstantMapData [1, 2, 1]) #@?= ptrue
        , -- paddressPubKeyCredentialAsData
          testCase "paddressPubKeyCredentialAsData on a pub key address" $
            Util.paddressPubKeyCredentialAsData
                # pconstant (Address (PubKeyCredential "deadbeef") Nothing)
                #@?= pconstant @(PAsData PPubKeyHash) "deadbeef"
        , testCase "paddressPubKeyCredential on a script address" $
            pfails $
                Util.paddressPubKeyCredentialAsData # pconstant (Address (ScriptCredential "deadbeef") Nothing)
        , -- pscriptHashFromAddress
          testCase "pscriptHashFromAddress on a pub key address" $
            pfails $
                Util.pscriptHashFromAddress # pconstant (Address (PubKeyCredential "deadbeef") Nothing)
        , testCase "pscriptHashFromAddress on a script address" $
            Util.pscriptHashFromAddress
                # pconstant (Address (ScriptCredential "deadbeef") Nothing)
                #@?= pconstant @PScriptHash "deadbeef"
        , -- pfiniteTxValidityRangeTimestamps
          testCase "finiteTxValidityRangeTimestamps on finite including bounds" $
            Util.pfiniteTxValidityRangeTimestamps (pconstant (Interval (LowerBound (Finite 0) True) (UpperBound (Finite 1) True)))
                #@?= pcon (PPair (pposixTime 0) (pposixTime 1))
        , testCase "finiteTxValidityRangeTimestamps on finite excluding bounds" $
            Util.pfiniteTxValidityRangeTimestamps (pconstant (Interval (LowerBound (Finite 0) False) (UpperBound (Finite 1) False)))
                #@?= pcon (PPair (pposixTime 0) (pposixTime 1))
        , testCase "finiteTxValidityRangeTimestamps on lower -inf" $
            pfails $
                Util.pfiniteTxValidityRangeTimestamps (pconstant (Interval (LowerBound NegInf False) (UpperBound (Finite 1) False)))
        , testCase "finiteTxValidityRangeTimestamps on lower +inf" $
            pfails $
                Util.pfiniteTxValidityRangeTimestamps (pconstant (Interval (LowerBound PosInf False) (UpperBound (Finite 1) False)))
        , testCase "finiteTxValidityRangeTimestamps on upper -inf" $
            pfails $
                Util.pfiniteTxValidityRangeTimestamps (pconstant (Interval (LowerBound (Finite 0) False) (UpperBound NegInf False)))
        , testCase "finiteTxValidityRangeTimestamps on upper +inf" $
            pfails $
                Util.pfiniteTxValidityRangeTimestamps (pconstant (Interval (LowerBound (Finite 0) False) (UpperBound PosInf False)))
        , -- pzip'
          testCase "zip' on [] []" $
            Util.pzip' # (pconvertLists # pconstant @(PBuiltinList PInteger) []) # pconstant @(PBuiltinList PInteger) [] #@?= (asPairs # pconstant [])
        , testCase "zip' on [1] []" $
            pfails $
                Util.pzip' # (pconvertLists # pconstant @(PBuiltinList PInteger) [1]) # pconstant @(PBuiltinList PInteger) []
        , testCase "zip' on [] [1]" $
            Util.pzip' # (pconvertLists # pconstant @(PBuiltinList PInteger) []) # pconstant @(PBuiltinList PInteger) [1] #@?= (asPairs # pconstant [])
        , testCase "zip' on [1] [2]" $
            Util.pzip' # (pconvertLists # pconstant @(PBuiltinList PInteger) [1]) # pconstant @(PBuiltinList PInteger) [2] #@?= (asPairs # pconstant [(1, 2)])
        , testCase "zip' on [1, 2] [3, 4]" $
            Util.pzip' # (pconvertLists # pconstant @(PBuiltinList PInteger) [1, 2]) # pconstant @(PBuiltinList PInteger) [3, 4] #@?= (asPairs # pconstant [(1, 3), (2, 4)])
        , -- pflatAssetsFromValue
          testCase "flatAssetsFromValue with a != ada, enough oil" $
            Util.pflatAssetsFromValue
                (pconstant (CurrencySymbol "a"), pconstant (TokenName "a"))
                (pconstant (CurrencySymbol "b"), pconstant (TokenName "b"))
                (pconstant (CurrencySymbol "h"), pconstant (TokenName "h"))
                ( pconstant @(PValue 'Unsorted 'NoGuarantees) $
                    singleton (CurrencySymbol "a") (TokenName "a") 1
                        <> singleton (CurrencySymbol "b") (TokenName "b") 2
                        <> singleton (CurrencySymbol "h") (TokenName "h") 3
                        <> singleton (CurrencySymbol "") (TokenName "") 2000000
                )
                2000000
                #@?= pcon (PFlatAssets 1 2 3)
        , testCase "flatAssetsFromValue with a != ada, not enough oil" $
            pfails $
                Util.pflatAssetsFromValue
                    (pconstant (CurrencySymbol "a"), pconstant (TokenName "a"))
                    (pconstant (CurrencySymbol "b"), pconstant (TokenName "b"))
                    (pconstant (CurrencySymbol "h"), pconstant (TokenName "h"))
                    ( pconstant @(PValue 'Unsorted 'NoGuarantees) $
                        singleton (CurrencySymbol "a") (TokenName "a") 1
                            <> singleton (CurrencySymbol "b") (TokenName "b") 2
                            <> singleton (CurrencySymbol "h") (TokenName "h") 3
                            <> singleton (CurrencySymbol "") (TokenName "") (2000000 - 1)
                    )
                    2000000
        , testCase "flatAssetsFromValue with a == ada, enough oil" $
            Util.pflatAssetsFromValue
                (pconstant (CurrencySymbol ""), pconstant (TokenName ""))
                (pconstant (CurrencySymbol "b"), pconstant (TokenName "b"))
                (pconstant (CurrencySymbol "h"), pconstant (TokenName "h"))
                ( pconstant @(PValue 'Unsorted 'NoGuarantees) $
                    singleton (CurrencySymbol "b") (TokenName "b") 2
                        <> singleton (CurrencySymbol "h") (TokenName "h") 3
                        <> singleton (CurrencySymbol "") (TokenName "") (5 + 2000000)
                )
                2000000
                #@?= pcon (PFlatAssets 5 2 3)
        , testCase "flatAssetsFromValue with a == ada, not enough oil" $
            pfails $
                Util.pflatAssetsFromValue
                    (pconstant (CurrencySymbol ""), pconstant (TokenName ""))
                    (pconstant (CurrencySymbol "b"), pconstant (TokenName "b"))
                    (pconstant (CurrencySymbol "h"), pconstant (TokenName "h"))
                    ( pconstant @(PValue 'Unsorted 'NoGuarantees) $
                        singleton (CurrencySymbol "b") (TokenName "b") 2
                            <> singleton (CurrencySymbol "h") (TokenName "h") 3
                            <> singleton (CurrencySymbol "") (TokenName "") (2000000 - 1)
                    )
                    2000000
        , -- pshareClassToken
          testCase "shareClassToken with an ada pool" $
            Util.pshareClassToken
                (pconstant (CurrencySymbol ""), pconstant (TokenName ""))
                (pconstant (CurrencySymbol "b"), pconstant (TokenName "b"))
                #@?= pcon (PTokenName (pconstant "\171\171eO\241\181\248%q@\EOTA\ETXc\SOH\ESC\225e\RS\168s\146\192\149\206\145\238\175\140\&9\"U"))
        , testCase "shareClassToken with a non-ada pool" $
            Util.pshareClassToken
                (pconstant (CurrencySymbol "a"), pconstant (TokenName "a"))
                (pconstant (CurrencySymbol "b"), pconstant (TokenName "b"))
                #@?= pcon (PTokenName (pconstant "\170\191C\202\154\142\214\130\USY\191\NAK\153 :\187\147A\232;,sAOJ\170\135\181`\138=\251"))
        ]

-- TODO: tests
main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    defaultMain tests
