# Deployment

## Overview
This subproject provides a small Bun script for deploying Plutus smart contracts using MeshJS and Blockfrost. The script includes functionality to create pools and requests, and the requests against the created pool, and check the current wallet balance.

## Prerequisites
Ensure you have the following installed:
- [Bun](https://bun.sh/) for running the script.
- A Blockfrost API key for interacting with the Cardano blockchain.
- A Cardano wallet mnemonic.

The Blockfrost API key and the Cardano wallet mnemonic are sourced from the gitignored `.env` file. You can find an example at `.env.example`.

## Usage
Run the script with:
```sh
bun run src/index.ts [command]
```

### Commands:
- **`bench <plutus.json> <request_count>`**: Deploy a pool and the provided number of requests and apply them against the pool.
The <plutus.json> must be a path to a CIP-0057 compliant contracts export, like the ones produced by this repository.

- **`list-balance`**: Lists the wallet balance.

### Example:
```sh
bun run src/index.ts bench ../plutus-tx/artifacts/plutus.json 1
```

## How It Works
1. **Creating a Pool and Requests**: The script reads a Plutus contract blueprint (`plutus.json`), initializes a wallet using MeshJS, and creates a pool along with a specified number of requests.
2. **Applying Requests**: After transactions are confirmed on-chain, the script applies requests to the pool.
3. **Listing Wallet Balance**: The script retrieves and prints the wallet's balance.

## Contact
For any questions or contributions, feel free to reach out through the project's repository.

