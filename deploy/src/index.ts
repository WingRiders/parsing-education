import fs from "fs";
import { OfflineEvaluator } from "@meshsdk/core-csl";
import {
  applyCborEncoding,
  BlockfrostProvider,
  deserializeAddress,
  MeshTxBuilder,
  MeshWallet,
  PlutusScript,
  resolveSlotNo,
  serializePlutusScript,
  Transaction,
} from "@meshsdk/core";
import assert from "assert";

const BLOCKFROST_API_KEY = process.env.BLOCKFROST_API_KEY;
assert(BLOCKFROST_API_KEY, "You must provide the BLOCKFROST_API_KEY env var");

const MNEMONIC = process.env.WALLET_MNEMONIC;
assert(MNEMONIC, "You must provide the WALLET_MNEMONIC env var");

const loadBlueprint = (filepath: string) =>
  JSON.parse(fs.readFileSync(filepath));

const aPolicyId = "";
const bPolicyId = "";
const aAssetName = "";
const bAssetName = "";

const asValidator = (blueprintValidator): PlutusScript => ({
  code: applyCborEncoding(blueprintValidator.compiledCode),
  version: "V3",
});

const applyRequests = async (
  blockchainProvider: BlockfrostProvider,
  blueprint: { validators: any[] },
  wallet: MeshWallet,
  poolUtxoInfo: { txHash: string; txIndex: number },
  requestsUtxoInfo: { txHash: string; txIndex: number }[],
) => {
  const requestBlueprintValidator = blueprint.validators.find((v) =>
    v.title.toLowerCase().includes("request"),
  );
  const requestHash = requestBlueprintValidator.hash;
  const requestValidator = asValidator(requestBlueprintValidator);
  const requestAddress = serializePlutusScript(requestValidator).address;

  const poolBlueprintValidator = blueprint.validators.find((v) =>
    v.title.toLowerCase().includes("pool"),
  );
  const poolHash = poolBlueprintValidator.hash;
  const poolValidator = asValidator(poolBlueprintValidator);
  const poolAddress = serializePlutusScript(poolValidator).address;

  const poolDatum = {
    alternative: 0,
    fields: [requestHash, aPolicyId, aAssetName, bPolicyId, bAssetName],
  };

  const collateral = (await wallet.getCollateral())[0];

  console.log("requestAddress", requestAddress);
  console.log("requestHash", requestHash);

  console.log("poolAddress", poolAddress);
  console.log("poolHash", poolHash);

  const evaluator = new OfflineEvaluator(blockchainProvider, "preprod");

  // https://meshjs.dev/apis/txbuilder
  let builder = new MeshTxBuilder({
    fetcher: blockchainProvider,
    verbose: true,
    evaluator: {
      evaluateTx: async (tx) => {
        const res = await evaluator.evaluateTx(tx, [], []);
        console.log("Evaluated", res);
        return res;
      },
    },
  })
    .setNetwork("preprod")
    .invalidBefore(
      Number(resolveSlotNo("preprod", new Date().getTime() - 60_000)),
    )
    .invalidHereafter(
      Number(resolveSlotNo("preprod", new Date().getTime() + 60_000)),
    )
    .spendingPlutusScriptV3()
    .txIn(
      poolUtxoInfo.txHash,
      poolUtxoInfo.txIndex,
      [{ unit: "lovelace", quantity: "2000000" }],
      poolAddress,
    )
    .txInInlineDatumPresent()
    .txInRedeemerValue({
      alternative: 0,
      fields: [requestsUtxoInfo.map((r) => r.txIndex)],
    })
    .txInScript(poolValidator.code)
    .txOut(poolAddress, [{ unit: "lovelace", quantity: "2000000" }])
    .txOutInlineDatumValue(poolDatum);

  const changeAddress = await wallet.getChangeAddress();
  const compensationAddress = wallet.getAddresses().enterpriseAddressBech32!;

  requestsUtxoInfo.forEach((requestUtxoInfo) => {
    builder = builder
      .spendingPlutusScriptV3()
      .txIn(
        requestUtxoInfo.txHash,
        requestUtxoInfo.txIndex,
        [{ unit: "lovelace", quantity: "6000000" }],
        requestAddress,
      )
      .txInInlineDatumPresent()
      .txInRedeemerValue({ alternative: 0, fields: [0] })
      .txInScript(requestValidator.code)
      .txOut(compensationAddress, [{ unit: "lovelace", quantity: "2000000" }]);
  });

  requestsUtxoInfo.forEach((_) => {
    builder = builder;
  });

  const tx = await builder
    .changeAddress(changeAddress)
    .txInCollateral(
      collateral.input.txHash,
      collateral.input.outputIndex,
      collateral.output.amount,
      collateral.output.address,
    )
    .complete();

  console.log(await blockchainProvider.evaluateTx(tx));
  const signedTx = await wallet.signTx(tx);
  const txHash = await wallet.submitTx(signedTx);
  console.log("Application tx:", txHash);
  return txHash;
};

const createPoolAndRequests = async (
  blueprint: { validators: any[] },
  wallet: MeshWallet,
  requestCount: number,
) => {
  const requestBlueprintValidator = blueprint.validators.find((v) =>
    v.title.toLowerCase().includes("request"),
  );
  const requestHash = requestBlueprintValidator.hash;
  const requestValidator = asValidator(requestBlueprintValidator);
  const requestAddress = serializePlutusScript(requestValidator).address;

  const poolBlueprintValidator = blueprint.validators.find((v) =>
    v.title.toLowerCase().includes("pool"),
  );
  const poolValidator = asValidator(poolBlueprintValidator);
  const poolAddress = serializePlutusScript(poolValidator).address;
  const poolDatum = {
    alternative: 0,
    fields: [requestHash, aPolicyId, aAssetName, bPolicyId, bAssetName],
  };

  const direction = { alternative: 0, fields: [] };
  const minWantedTokens = 0;
  const requestAction = {
    alternative: 0,
    fields: [direction, minWantedTokens],
  };

  const deadline = new Date().getTime() + 60_000 * 60;

  const address = deserializeAddress(await wallet.getChangeAddress());
  const ownerAddress = {
    alternative: 0,
    fields: [
      { alternative: 0, fields: [address.pubKeyHash] },
      { alternative: 1, fields: [] },
    ],
  };
  const beneficiary = ownerAddress;

  const requestDatum = {
    alternative: 0,
    fields: [
      beneficiary,
      ownerAddress,
      deadline,
      aPolicyId,
      aAssetName,
      bPolicyId,
      bAssetName,
      requestAction,
    ],
  };

  let tx = new Transaction({ initiator: wallet }).sendLovelace(
    { address: poolAddress, datum: { value: poolDatum, inline: true } },
    "2000000",
  );

  const requests: { txIndex: number }[] = [];
  for (let i = 0; i < requestCount; i++) {
    tx = tx.sendLovelace(
      {
        address: requestAddress,
        datum: { value: requestDatum, inline: true },
      },
      "6000000",
    );
    requests.push({ txIndex: i + 1 });
  }

  const unsignedTx = await tx.build();
  const signedTx = await wallet.signTx(unsignedTx);
  const txHash = await wallet.submitTx(signedTx);
  console.log("Pool & requests tx:", txHash);
  return {
    pool: { txHash, txIndex: 0 },
    requests: requests.map((r) => ({ ...r, txHash })),
  };
};

const main = async () => {
  const action = process.argv[2];
  if (action != "bench" && action != "list-balance") {
    console.log(
      "Usage: bun run index.js [bench <plutus.json> <request_count> | list-balance]",
    );
    return;
  }

  const blockchainProvider = new BlockfrostProvider(BLOCKFROST_API_KEY);
  blockchainProvider.fetchProtocolParameters();

  const wallet = new MeshWallet({
    networkId: 0,
    fetcher: blockchainProvider,
    submitter: blockchainProvider,
    key: { type: "mnemonic", words: MNEMONIC.split(" ") },
  });
  await wallet.init();

  if (action == "bench") {
    assert(process.argv.length == 5);
    const filepath = process.argv[3];
    const requestCount = Number(process.argv[4]);
    const blueprint = loadBlueprint(filepath);

    const utxosInfo = await createPoolAndRequests(
      blueprint,
      wallet,
      requestCount,
    );

    // const utxosInfo = {
    //   pool: {
    //     txHash:
    //       "0adf9751173835e073d3551ba42d2ecf875c662d84863357871ea61bb2ac6ae0",
    //     txIndex: 0,
    //   },
    //   requests: [
    //     {
    //       txIndex: 1,
    //       txHash:
    //         "0adf9751173835e073d3551ba42d2ecf875c662d84863357871ea61bb2ac6ae0",
    //     },
    //   ],
    // };

    console.log(utxosInfo);
    blockchainProvider.onTxConfirmed(utxosInfo.pool.txHash, async () => {
      await applyRequests(
        blockchainProvider,
        blueprint,
        wallet,
        utxosInfo.pool,
        utxosInfo.requests,
      );
    });
  } else if (action == "list-balance") {
    assert(process.argv.length == 3);
    const address = await wallet.getChangeAddress();
    console.log(address);
    const balance = await wallet.getBalance();
    console.log(balance);
  }
};

main();
