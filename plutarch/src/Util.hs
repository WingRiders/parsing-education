module Util where

import Plutarch.LedgerApi.V3
import Plutarch.LedgerApi.Value (padaSymbol, padaToken, pvalueOf)
import Plutarch.Prelude
import Types (PFlatAssets (PFlatAssets))

perrorIfFalse :: Term s (PBool :--> PUnit)
perrorIfFalse = plam $ \b -> pif b (pconstant ()) perror

paddressPubKeyCredentialAsData :: Term s (PAddress :--> PAsData PPubKeyHash)
paddressPubKeyCredentialAsData = phoistAcyclic $ plam $ \address ->
    pmatch address $ \addr ->
        pmatch (paddress'credential addr) $ \case
            PPubKeyCredential f -> f
            PScriptCredential _ -> ptraceInfoError "pubkey"

pextractNthInputValidatorHash :: Term s (PScriptContext :--> PInteger :--> PScriptHash)
pextractNthInputValidatorHash = phoistAcyclic $ plam $ \context i -> unTermCont $ do
    txOutResolved <- pmatchC (pextractNthTxInput # context # i)
    txOut <- pmatchC txOutResolved.ptxInInfo'resolved
    address <- pmatchC txOut.ptxOut'address
    let vhash = pmatch address.paddress'credential $ \case
            PScriptCredential vhashData -> vhashData
            PPubKeyCredential _ -> ptraceInfoError "nth"
    pure $ pfromData vhash

pextractNthTxInput :: Term s (PScriptContext :--> PInteger :--> PTxInInfo)
pextractNthTxInput = phoistAcyclic $ plam $ \context index -> unTermCont $ do
    ctx <- pmatchC context
    info <- pmatchC ctx.pscriptContext'txInfo
    let inputs = pfromData info.ptxInfo'inputs
    pure $ pfromData $ pelemAt # index # inputs

{- | Expand the given list of conditions with pand'.
Evaluates the arguments strictly.
-}
pallOf :: [Term s PBool] -> Term s PBool
pallOf = foldr1 (\res x -> pand' # res # x)

{- |
 Alternative to zip, throwing error when there is not enough elements in `b`.
 Used in pool validator, when zipping requests with compensation tx outputs. There needs to be
 a tx output for every request.
-}
pzip' ::
    (PElemConstraint PList a, PElemConstraint PBuiltinList b) =>
    Term s (PList a :--> PBuiltinList b :--> PList (PPair a b))
pzip' = phoistAcyclic $ pfix #$ plam $ \recur la lb ->
    pelimList
        ( \x xs ->
            pelimList
                (\y ys -> pcons # pcon (PPair x y) # (recur # xs # ys))
                -- Each element from the first list must have a corresponding element
                (ptraceInfoError "zip")
                lb
        )
        -- We allow leftovers in the second list
        pnil
        la

{- |
 Extracts PFlatAssets from value (minus oilAda).

 Warning: this ignores any other tokens in the value!
-}
pflatAssetsFromValue ::
    (Term s PCurrencySymbol, Term s PTokenName) ->
    (Term s PCurrencySymbol, Term s PTokenName) ->
    (Term s PCurrencySymbol, Term s PTokenName) ->
    Term s (PValue sort guarantee) ->
    Term s PInteger ->
    Term s PFlatAssets
pflatAssetsFromValue (assetACs, assetATn) (assetBCs, assetBTn) (shareCs, shareTn) val oilAda =
    -- Here we assume that `val` is non-negative (which is the case for TxOut value) as well as oilAda >=0
    pcon $ PFlatAssets qtyA qtyB qtyShares
  where
    qtyA = pvalueOfWithOilCheck # val # assetACs # assetATn # oilAda
    qtyB = pvalueOf # val # assetBCs # assetBTn
    qtyShares = pvalueOf # val # shareCs # shareTn

{- |
 Get the quantity of the given asset in the 'PValue' while checking
 that there is enough oil in the value. If asset is ada, returned quantity is without oil.

 Note: This function throws if oil ada does not check up.
-}
pvalueOfWithOilCheck :: Term s (PValue sort guarantee :--> PCurrencySymbol :--> PTokenName :--> PInteger :--> PInteger)
pvalueOfWithOilCheck = phoistAcyclic $ plam $ \val policyId name oilAda -> unTermCont $ do
    assetIsAda <- pletC (policyId #== padaSymbol)

    qtyAdaWithoutOil <- pletC ((pvalueOf # val # padaSymbol # padaToken) - oilAda)

    let check = pif assetIsAda (qtyAdaWithoutOil #>= 0) (qtyAdaWithoutOil #== 0)
        qty = pif assetIsAda qtyAdaWithoutOil (pvalueOf # val # policyId # name)
    pure $ pif check qty (ptraceInfoError "oil")

pscriptHashFromAddress :: Term s (PAddress :--> PScriptHash)
pscriptHashFromAddress = phoistAcyclic $ plam $ \addr -> unTermCont $ do
    address <- pmatchC addr
    pure $ pmatch address.paddress'credential $ \case
        PScriptCredential c -> pfromData c
        _else -> ptraceInfoError "hash"

pisScriptAddress :: Term s (PAddress :--> PBool)
pisScriptAddress = phoistAcyclic $ plam $ \addr -> unTermCont $ do
    address <- pmatchC addr
    pure $ pmatch address.paddress'credential $ \case
        PScriptCredential _ -> pconstant True
        _else -> pconstant False

pmapToPList ::
    (PElemConstraint PBuiltinList a, PElemConstraint PList b) =>
    Term s ((a :--> b) :--> PBuiltinList a :--> PList b)
pmapToPList = phoistAcyclic $ plam $ \f l ->
    precList (\recur x xs -> pcons # (f # x) # (recur # xs)) (const pnil) # l

pcontainsDuplicate :: Term s (PBuiltinList (PAsData PInteger)) -> Term s PBool
pcontainsDuplicate l = (pfix # plam f) # l # (pnil @PList)
  where
    f recur l' xs =
        pelimList
            ( \index indices ->
                pif
                    (pelem # index # xs)
                    (pconstant True)
                    (recur # indices # (pcons # index # xs))
            )
            (pconstant False)
            l'

pfiniteTxValidityRangeTimestamps :: Term s (PInterval PPosixTime) -> Term s (PPair PPosixTime PPosixTime)
pfiniteTxValidityRangeTimestamps r = pmatch r $ \case
    PInterval from to -> pmatch from $ \case
        PLowerBound lower _ -> pmatch lower $ \case
            PFinite startTime -> pmatch to $ \case
                PUpperBound upper _ -> pmatch upper $ \case
                    PFinite endTime -> pcon $ PPair (pfromData startTime) (pfromData endTime)
                    _else -> ptraceInfoError "finiterange"
            _else -> ptraceInfoError "finiterange"

{- |
Share class token is obtained with the following formula:

sha3_256(sha3_256(token a) <> sha3_256(token b))

This gives a unique token name per each token pair.
-}
pshareClassToken :: (Term s PCurrencySymbol, Term s PTokenName) -> (Term s PCurrencySymbol, Term s PTokenName) -> Term s PTokenName
pshareClassToken (aSymbol, aToken) (bSymbol, bToken) =
    pcon . PTokenName $ psha3_256 #$ passetClassHash aSymbol aToken <> passetClassHash bSymbol bToken
  where
    passetClassHash cs tn = psha3_256 # (pto cs <> pto tn)

ptxSignedBy :: Term s (PTxInfo :--> PAsData PPubKeyHash :--> PBool)
ptxSignedBy = phoistAcyclic $ plam $ \txInfo hash -> pmatch txInfo $ \t ->
    pelem # hash # pfromData t.ptxInfo'signatories
