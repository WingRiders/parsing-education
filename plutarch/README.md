# Plutarch Parsing Contracts

## Overview
This project includes an adapted version of the Wingriders DEX V1/V2 request parsing logic. It is algorithmically identical to the `../plutus-tx/` codebase. The two exported validators (pool and request) can be used standalone and don't depend on anything outside of this repository. The contracts have been updated to the latest Plutarch version and compile down to Plutus V3. The exporting is CIP-0057 compliant. The project uses nix and cabal.

## Project Structure
- **`./src`**: Contains the source code for the project, including constants, request parsing logic, types, and utility functions.
- **`./test`**: Includes unit tests to verify the correctness of the parsing contracts, tests the utility functions.
- **`./app`**: Contains the contracts blueprint exporting machinery.
- **`./artifacts/plutus.json`**: Contains the generated blueprint file.

## Build and Development
### Prerequisites
Follow [these instructions](https://github.com/input-output-hk/iogx/blob/main/doc/nix-setup-guide.md) to install and configure nix, even if you already have it installed. Then you can enter the nix shell with:
```sh
nix develop
```

### Building the Project
To build the project from inside the nix shell, use:
```sh
make
```
To export the contracts:
```sh
make blueprints
```

### Running Tests
Unit tests are located in the `./test` directory. Assuming you're in the nix shell, run the tests with:
```sh
make test
```

## Deployment
Once you have the blueprint in the `./artifacts/plutus.json`, you can navigate into the `../deploy/` directory and consult the `../deploy/README.md` file for how to deploy the contracts.

An example transaction with 1 applied request: https://preprod.cardanoscan.io/transaction/28322f49db7c6c90c29ffe06bd5414127f036b5ad76e943389ba9f7ead8e8399

An example transaction with 2 applied requests:
https://preprod.cardanoscan.io/transaction/f1180270fe841dad872fef46fd93e10f8d634ab0b20e2d0264458014239539a0

An example transaction with 4 applied requests: 
https://preprod.cardanoscan.io/transaction/19a54912f51b9bf2fd3616911767aa016ca3a7fcac64ad1560fc4275674af8e9

An example transaction with 10 applied requests:
https://preprod.cardanoscan.io/transaction/07e90862c39548bc7bc973b18212ab2c1040120ae137617e7d44bc1719a8a1f9

An example transaction with 20 applied requests:
https://preprod.cardanoscan.io/transaction/0d983bab4a302efc16fa28be3ce2f819627bc2dd2957d16e3298aba09b7ce87a

An example transaction with 24 applied requests:
https://preprod.cardanoscan.io/transaction/5ee02d0f6ccc350152530026276681ccca81a8e270596db2d41101c57c610373

An example transaction with 30 applied requests:
https://preprod.cardanoscan.io/transaction/86a5c6567b5c2f9f3e060c5aa9c843a3df98e482f48ab677141fb6b5d7da0e29

An example transaction with 38 applied requests (the current max):
https://preprod.cardanoscan.io/transaction/cb2b5cef07a3f6ec31de8c9fccf31e67254573e3e582f2883fc5ea29c4dc3dc7

## Contact
For any inquiries or support, please reach out via the project's repository or associated communication channels.
