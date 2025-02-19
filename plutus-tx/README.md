# PlutusTx Parsing Contracts

## Overview
This project includes an adapted version of the Wingriders DEX V1 request parsing logic. The contracts have been updated to the latest Plinth version and compile down to Plutus V3. They are standalone and don't depend on the Wingriders code/infrastructure outside this repository. The exporting is CIP-0057 compliant. The project uses nix and cabal.

## Project Structure
- **`./src`**: Contains the source code for the project, including constants, request parsing logic, types, and utility functions.
- **`./test`**: Includes unit tests to verify the correctness of the parsing contracts, tests the utility functions.
- **`./app`**: Contains the contracts blueprint exporting machinery.
- **`./nix`**: Nix configuration files for setting up the development environment.
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

An example transaction with 1 applied request: https://preprod.cardanoscan.io/transaction/e712d3e68556e1dfb3d3a86ee64dbd765bedaebd265ca612840a302603c5c8e2

An example transaction with 2 applied requests:
https://preprod.cardanoscan.io/transaction/036ec458ba9ba6ae873941246a912223d4cc1af3c24c811e18280ccaad6545d4

An example transaction with 4 applied requests (current max):
https://preprod.cardanoscan.io/transaction/7511201af041f9579ba67469740e8d1754b5e8e1fb1fbfb55e519f531c0ed83c

## Contact
For any inquiries or support, please reach out via the project's repository or associated communication channels.

