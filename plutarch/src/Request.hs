module Request where

import Plutarch.LedgerApi.V3
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Types
import Util

-- | Reclaims are allowed if the request is signed by the request owner
pvalidateReclaim :: Term s PRequestDatum -> Term s PScriptContext -> Term s PBool
pvalidateReclaim datum context = unTermCont $ do
    c <- pmatchC context
    d <- pmatchC datum
    let signedByOwner = ptraceInfoIfFalse "signedByOwner" $ ptxSignedBy # c.pscriptContext'txInfo # ownerHash
        ownerHash = paddressPubKeyCredentialAsData # pfromData d.ownerAddress
    pure signedByOwner

-- | Applications are allowed if there's a pool input on the specified location
pvalidateApply :: Term s PRequestConfig -> Term s PScriptContext -> Term s PInteger -> Term s PBool
pvalidateApply requestConfig context poolIndex = unTermCont $ do
    conf <- pmatchC requestConfig
    let poolIsCorrect = ptraceInfoIfFalse "poolIsCorrect" $ delegateeValidatorHash #== pfromData conf.pool
        delegateeValidatorHash = pextractNthInputValidatorHash # context # poolIndex
    pure poolIsCorrect

prequestScriptValidator ::
    Term s PRequestConfig ->
    Term s PRequestDatum ->
    Term s PRequestRedeemer ->
    Term s PScriptContext ->
    Term s PBool
prequestScriptValidator config datum redeemer context = pmatch redeemer $ \case
    PApply poolIndex -> pvalidateApply config context (pfromData poolIndex)
    PReclaim -> pvalidateReclaim datum context

requestValidator :: Term s (PRequestConfig :--> PData :--> PUnit)
requestValidator = plam $ \config context ->
    perrorIfFalse
        #$ plet (punsafeCoerce @PScriptContext context)
        $ \ctx -> pmatch ctx $
            \c -> pmatch c.pscriptContext'scriptInfo $ \case
                PSpendingScript _outref maybeDatum -> pmatch maybeDatum $ \case
                    PDJust datum ->
                        let d = punsafeCoerce @PRequestDatum datum
                            r = punsafeCoerce @PRequestRedeemer c.pscriptContext'redeemer
                         in prequestScriptValidator config d r ctx
                    _else -> ptraceInfoError "nodatum"
                _else -> ptraceInfoError "invalidinfo"
