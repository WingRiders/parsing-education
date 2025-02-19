# Aiken Smart Contracts

## Overview
This project provides smart contracts written in [Aiken](https://aiken-lang.org/), including pool and request validators which are standalone versions of the Wingriders DEX V1/V2 request parsing. This subprojecs in algorithmically identical to the `../plutarch/` and `../plutus-tx/` versions. The contracts are rewritten with Aiken 1.1.12 and compile down to Plutus V3. The exporting is CIP-0058 compliant.

## Project Structure
- **`lib/`**: Contains the source code for constants, types, utility functions and tests.
- **`validators/`**: Contains the source code for the request and pool validators.

## Prerequisites
To use this project, ensure you have:
- [Aiken 1.1.12](https://aiken-lang.org/) installed OR.
- [Aikup](https://github.com/aiken-lang/aikup) installed.
With Aikup you can install the correct compiler version from the projecs folder with:
```sh
aikup
```

## Building the Project
Compile Aiken contracts with traces using:
```sh
aiken build -t verbose
```
Note that the built contracts don't have the compile-time configuration applied yet. To apply the configuration you can use the `aiken blueprint apply` subcommand.
First apply the pool configuration:
```sh
aiken blueprint apply -v pool-o plutus.applie.json
```
The `validity_policy_id`, `staking_rewards_policy_id` and `enforced_script_output_datum_hash` are `0000000000000000000000000000000000000000` to mimic the plutus-tx and plutarch versions. The `treasury_holder_script_hash` is `00000000000000000000000000000000000000000000000000000000000000000000000000000000`.
Next apply the request configuration:
```sh
aiken blueprint apply -v request -i plutus.applied.json -o plutus.applied.json
```
The pool hash is the one we obtained in the previous step: `88733143e8b0f86ab726224b4ae8b9804614041eeb8fb5ee62b0f01b`

## Running Tests
Run tests with:
```sh
aiken check
```

## Deployment
Once you have the blueprint with applied parameters in the `./plutus.json`, you can navigate into the `../deploy/` directory and consult the `../deploy/README.md` file for how to deploy the contracts.

An example transaction with 1 applied request:
https://preprod.cardanoscan.io/transaction/9265f197826b710af260100884428286a7062cd2980013d46780c89310fe6daf

An example transaction with 2 applied requests:
https://preprod.cardanoscan.io/transaction/e4b00e4034876df06d4b1471a10b47d9cbf6a93d448ef178b712091a9dad3bf8

An example transaction with 4 applied requests:
https://preprod.cardanoscan.io/transaction/c63da4ce0bbb4c16c2eebb6c61d2515d8f5e6b519fa0ffe4f67ef7e0eb692fe6

An example transaction with 10 applied requests:
https://preprod.cardanoscan.io/transaction/95a49f7cbd3d868303cd90c409bd929066e2c4877ff34b49508550be3a490c6c

An example transaction with 20 applied requests:
https://preprod.cardanoscan.io/transaction/a320d86221ae74da20efd0cf88e2e394b1e78c4f80e2f526552b239d37e477c2

An example transaction with 24 applied requests (the current max):
https://preprod.cardanoscan.io/transaction/f39fdfdfccc08ddaebdad711d5f1d4580c58172638e97c1f232d1d6c5daae7e1

## Contact
For any inquiries or support, please reach out via the project's repository or associated communication channels.

