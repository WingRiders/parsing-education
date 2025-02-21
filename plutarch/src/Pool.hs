module Pool where

import Constants
import Plutarch.LedgerApi.AssocMap (plookup)
import Plutarch.LedgerApi.V3
import Plutarch.LedgerApi.Value
import Plutarch.Maybe (pfromJust)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Types
import Util

{- |
Pool evolve transactions include one pool input and 1+ request inputs.
The pool does the bulk of the validation.
It scans over the requests, parses them and makes sure each is compensated.
-}
pvalidatePoolEvolve ::
    Term s PPoolConfig ->
    Term s PPoolDatum ->
    Term s (PBuiltinList (PAsData PInteger)) ->
    Term s PScriptContext ->
    Term s PBool
pvalidatePoolEvolve poolConfig datum requestIndices' context = unTermCont $ do
    ctx <- pmatchC context
    -- plet since it's used more that once
    requestIndices <- pletC requestIndices'
    datumFields <- pmatchC datum
    requestHash <- pletC $ pfromData datumFields.requestHash
    aSymbol <- pletC $ pfromData datumFields.aSymbol
    aToken <- pletC $ pfromData datumFields.aToken
    bSymbol <- pletC $ pfromData datumFields.bSymbol
    bToken <- pletC $ pfromData datumFields.bToken

    configFields <- pmatchC poolConfig
    shareSymbol <- pletC $ pfromData configFields.validitySymbol
    shareToken <- pletC $ pshareClassToken (aSymbol, aToken) (bSymbol, bToken)

    txInfoFields <- pmatchC ctx.pscriptContext'txInfo

    PPair _ txValidityEnd <- pmatchC $ pfiniteTxValidityRangeTimestamps txInfoFields.ptxInfo'validRange

    let noRequestDuplicated = pnot #$ pcontainsDuplicate requestIndices

        orderedInputs = pmapToPList # plam (\i -> pfromData $ pelemAt # pfromData i # pfromData txInfoFields.ptxInfo'inputs) # requestIndices
        -- the first output is the pool, the rest are compensations
        orderedOutputs = ptail # pfromData txInfoFields.ptxInfo'outputs
        parsedRequests =
            parseRequestsAndValidateMetadata
                configFields
                (aSymbol, aToken)
                (bSymbol, bToken)
                (shareSymbol, shareToken)
                requestHash
                txValidityEnd
                (pfromData txInfoFields.ptxInfo'data)
                orderedInputs
                orderedOutputs
    pure $
        pallOf
            [ -- No request index can be duplicated by the agent, otherwise funds can be stolen
              ptraceInfoIfFalse "noRequestDuplicated" $ noRequestDuplicated
            , -- There is at least one processed request
              ptraceInfoIfFalse "requestsCount" $ (plength # parsedRequests) #> 0
            ]

parseRequestsAndValidateMetadata ::
    PPoolConfig s ->
    (Term s PCurrencySymbol, Term s PTokenName) ->
    (Term s PCurrencySymbol, Term s PTokenName) ->
    (Term s PCurrencySymbol, Term s PTokenName) ->
    Term s PScriptHash ->
    Term s PPosixTime ->
    Term s (PMap 'Unsorted PDatumHash PDatum) ->
    Term s (PList PTxInInfo) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PList PParsedRequest)
parseRequestsAndValidateMetadata
    configFields
    (aSymbol, aToken)
    (bSymbol, bToken)
    (shareSymbol, shareToken)
    requestHash
    txValidityEnd
    datums
    orderedInputs
    orderedOutputs = pmap # plam parse #$ pzip' # orderedInputs # orderedOutputs
      where
        parse p = pmatch p $ \(PPair input output) -> unTermCont $ do
            inputResolved <- pletC $ pmatch input ptxInInfo'resolved
            inputFields <- pmatchC inputResolved
            requestValue <- pletC $ pfromData inputFields.ptxOut'value

            outputFields <- pmatchC $ pfromData output
            compensationValue <- pletC $ pfromData outputFields.ptxOut'value

            datumFields <- pmatchC $ punsafeRequestDatum inputResolved datums

            let rAssets =
                    pflatAssetsFromValue
                        (aSymbol, aToken)
                        (bSymbol, bToken)
                        (shareSymbol, shareToken)
                        requestValue
                        (requestOilAda + agentFeeAda)
                cAssets =
                    pflatAssetsFromValue
                        (aSymbol, aToken)
                        (bSymbol, bToken)
                        (shareSymbol, shareToken)
                        compensationValue
                        requestOilAda

                actualValidatorHash = pscriptHashFromAddress # inputFields.ptxOut'address

                expectedOutDatumHash =
                    pif
                        (pisScriptAddress # pfromData datumFields.beneficiary)
                        -- The fixed output datum hash for script compensations
                        (pcon $ POutputDatumHash configFields.enforcedScriptOutputDatumHash)
                        -- And no output datum for pub key compensations
                        (pcon PNoOutputDatum)

                actionIsAllowed = pmatch (pfromData datumFields.action) $ \case
                    PExtractTreasury ->
                        -- Treasury must be extracted to a treasury script
                        pscriptHashFromAddress # pfromData datumFields.beneficiary #== pfromData configFields.treasuryHolderScriptHash
                    PAddStakingRewards ->
                        -- Staking rewards are additionally authorized
                        pisAuthorizedToAddStakingRewards
                            configFields
                            aSymbol
                            shareToken
                            requestValue
                            compensationValue
                    _else -> pconstant True

            pure $
                pif
                    ( pallOf
                        [ -- The output address must be the request beneficiary
                          ptraceInfoIfFalse "out address" $ outputFields.ptxOut'address #== pfromData datumFields.beneficiary
                        , -- The request application must happen before the deadline
                          ptraceInfoIfFalse "range" $ txValidityEnd #< pfromData datumFields.deadline
                        , -- The input actually has the request hash
                          ptraceInfoIfFalse "request hash" $ actualValidatorHash #== requestHash
                        , -- The token A currency symbol matches
                          ptraceInfoIfFalse "asymbol" $ pfromData datumFields.assetASymbol #== aSymbol
                        , -- The token A asset name matches
                          ptraceInfoIfFalse "atoken" $ pfromData datumFields.assetAToken #== aToken
                        , -- The token B currency symbol matches
                          ptraceInfoIfFalse "bsymbol" $ pfromData datumFields.assetBSymbol #== bSymbol
                        , -- The token B asset name matches

                          ptraceInfoIfFalse "btoken" $ pfromData datumFields.assetBToken #== bToken
                        , -- the compensation has the expected out datum
                          ptraceInfoIfFalse "outdatum" $ outputFields.ptxOut'datum #== expectedOutDatumHash
                        , -- the action is actually allowed

                          ptraceInfoIfFalse "allowed" actionIsAllowed
                        ]
                    )
                    (pcon $ PParsedRequest (pfromData datumFields.action) rAssets cAssets)
                    (ptraceInfoError "parse")

{- |
 Checks whether the add staking rewards request is authorized
  1. The pool has to be an ADA <> token pool
  2. The request contains a staking rewards token with the tokenName matching the LP assets
  3. The reward token was returned as a compensation
-}
pisAuthorizedToAddStakingRewards ::
    PPoolConfig s ->
    Term s PCurrencySymbol ->
    Term s PTokenName ->
    Term s (PValue sort guarantee) ->
    Term s (PValue sort guarantee) ->
    Term s PBool
pisAuthorizedToAddStakingRewards
    configFields
    aSymbol
    shareToken
    requestValue
    compensationValue =
        pallOf
            [ ptraceInfoIfFalse "ada" $ padaSymbol #== aSymbol
            , ptraceInfoIfFalse "reward token" $ hasRewardToken
            , ptraceInfoIfFalse "return reward token" $ didReturnRewardToken
            ]
      where
        rewardSymbol = pfromData configFields.stakingRewardsSymbol
        poolId = shareToken

        hasRewardToken = pvalueOf # requestValue # rewardSymbol # poolId #== 1
        didReturnRewardToken = pvalueOf # compensationValue # rewardSymbol # poolId #== 1

punsafeRequestDatum :: Term s PTxOut -> Term s (PMap 'Unsorted PDatumHash PDatum) -> Term s PRequestDatum
punsafeRequestDatum txOut txDatums = datum
  where
    datum = punsafeCoerce @PRequestDatum d
    -- Both inline and hash are supported
    d = pmatch (pmatch txOut ptxOut'datum) $ \case
        POutputDatum dat -> dat
        POutputDatumHash hash -> pfromJust #$ plookup # pfromData hash # txDatums
        PNoOutputDatum -> perror

ppoolScriptValidator ::
    Term s PPoolConfig ->
    Term s PPoolDatum ->
    Term s PPoolRedeemer ->
    Term s PScriptContext ->
    Term s PBool
ppoolScriptValidator config datum redeemer context = pmatch redeemer $ \case
    PEvolve requestIndices -> pvalidatePoolEvolve config datum (pfromData requestIndices) context

poolValidator :: Term s (PPoolConfig :--> PData :--> PUnit)
poolValidator = plam $ \config context ->
    perrorIfFalse
        #$ plet (punsafeCoerce @PScriptContext context)
        $ \ctx ->
            pmatch ctx $ \c -> pmatch c.pscriptContext'scriptInfo $ \case
                PSpendingScript _outref maybeDatum -> pmatch maybeDatum $ \case
                    PDJust datum ->
                        let d = punsafeCoerce @PPoolDatum datum
                            r = punsafeCoerce @PPoolRedeemer c.pscriptContext'redeemer
                         in ppoolScriptValidator config d r ctx
                    _else -> ptraceInfoError "nodatum"
                _else -> ptraceInfoError "invalidinfo"
