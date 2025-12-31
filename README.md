# idris2-evm

EVM execution with coverage collection for Idris2 smart contracts.

## Overview

`idris2-evm` is a CLI tool that:

1. Compiles Idris2 contracts to Yul/EVM bytecode (via `idris2-yul`)
2. Deploys to a local Anvil instance
3. Executes test transactions
4. Collects execution traces
5. Outputs coverage data compatible with `idris2-coverage`

## Requirements

- [Foundry](https://getfoundry.sh) (anvil, cast) - Local EVM and tooling
- [solc](https://docs.soliditylang.org/) - Solidity compiler (for Yul compilation)
- [idris2-yul](https://github.com/example/idris2-yul) (optional) - For `.idr` file compilation
- [pack](https://github.com/stefan-hoeck/idris2-pack) - Idris2 package manager

## Installation

```bash
# Clone the repository
git clone https://github.com/example/idris2-evm
cd idris2-evm

# Build with pack
pack build idris2-evm

# Or install globally
pack install-app idris2-evm
```

## Usage

```bash
idris2-evm-run [options] <contract.idr|contract.yul>
```

### Options

| Option | Description |
|--------|-------------|
| `-h, --help` | Show help message |
| `-v, --version` | Show version |
| `--verbose` | Verbose output |
| `--evm-version <ver>` | EVM version: `cancun`, `shanghai`, `osaka` (default: `cancun`) |
| `-o, --output <path>` | Output file for coverage data |
| `--format <fmt>` | Output format: `json`, `chez`, `csv` (default: `json`) |
| `--tests <path>` | File containing test calls (one per line) |
| `--call <calldata>` | Add a test call (hex calldata) |

### Examples

```bash
# Run a Yul contract with default tests
idris2-evm-run test/Counter.yul

# Run with specific test calls
idris2-evm-run --call 0x371303c0 --call 0x06661abd Counter.yul

# Output coverage to file
idris2-evm-run -o coverage.json Counter.idr

# Use a test file
idris2-evm-run --tests tests.txt Counter.idr
```

## Output Formats

### JSON (default)

```json
{
  "timestamp": 1704000000,
  "total_functions": 5,
  "executed_functions": 3,
  "coverage_percent": 60,
  "function_hits": [
    {"name": "Counter.increment", "hits": 2},
    {"name": "Counter.decrement", "hits": 1}
  ]
}
```

### Chez Scheme Profile

Compatible with `idris2-coverage` parsing infrastructure:

```scheme
; idris2-evm profile data
("Counter.increment" . 2)
("Counter.decrement" . 1)
```

### CSV

```csv
function,hit_count
Counter.increment,2
Counter.decrement,1
```

## Architecture

```
src/
├── Main.idr           # CLI entry point and argument parsing
└── EVM/
    ├── Types.idr      # Core types (Address, Word256, traces, configs)
    ├── Executor.idr   # High-level execution orchestration
    ├── Foundry.idr    # Foundry toolchain integration (anvil, cast)
    ├── Trace.idr      # Trace parsing and source mapping
    ├── Profiler.idr   # Coverage profile generation
    ├── Word256.idr    # 256-bit word arithmetic
    ├── Stack.idr      # EVM stack operations
    ├── Memory.idr     # EVM memory model
    └── Storage.idr    # EVM storage model
```

## Integration with idris2-coverage

The output is designed to integrate with [idris2-coverage](https://github.com/example/idris2-coverage):

```bash
# Run EVM tests and generate coverage
idris2-evm-run -o evm-coverage.json MyContract.idr

# Merge with static analysis from dumpcases
idris2-coverage merge --evm evm-coverage.json --static dumpcases.json
```

## Development

### Dependencies

- `base` - Idris2 standard library
- `contrib` - Idris2 contrib library
- `idris2-coverage` - Coverage analysis library

### Building

```bash
pack build idris2-evm
```

### Testing

```bash
# Start anvil (if not running)
anvil &

# Run the test contract
pack run idris2-evm test/Counter.yul
```

## License

MIT

## Authors

LazyEvm Team
