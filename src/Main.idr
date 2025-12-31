||| idris2-evm CLI - Pure Idris2 EVM Interpreter
|||
||| This tool executes EVM bytecode through a pure Idris2 interpreter.
||| When compiled with --coverage, the Chez Scheme profiler tracks
||| which interpreter branches are executed, giving semantic coverage
||| of the EVM code.
|||
||| Usage:
|||   idris2-evm-run [options] <bytecode.hex>
|||   idris2-evm-run --calldata 0x371303c0 <bytecode.hex>
module Main

import EVM.Word256
import EVM.Stack
import EVM.Memory
import EVM.Storage
import EVM.Opcodes
import EVM.Bytecode
import EVM.Interpreter

import System
import System.File
import Data.List
import Data.String

%default covering

-- =============================================================================
-- CLI Options
-- =============================================================================

record Options where
  constructor MkOptions
  bytecodeFile : Maybe String
  bytecodeHex : Maybe String
  calldataHex : String
  gasLimit : Nat
  showHelp : Bool
  showVersion : Bool
  verbose : Bool
  disassemble : Bool

defaultOptions : Options
defaultOptions = MkOptions Nothing Nothing "" 1000000 False False False False

-- =============================================================================
-- Argument Parsing
-- =============================================================================

parseArgs : List String -> Options -> Options
parseArgs [] opts = opts
parseArgs ("--help" :: rest) opts = parseArgs rest ({ showHelp := True } opts)
parseArgs ("-h" :: rest) opts = parseArgs rest ({ showHelp := True } opts)
parseArgs ("--version" :: rest) opts = parseArgs rest ({ showVersion := True } opts)
parseArgs ("-v" :: rest) opts = parseArgs rest ({ showVersion := True } opts)
parseArgs ("--verbose" :: rest) opts = parseArgs rest ({ verbose := True } opts)
parseArgs ("--disassemble" :: rest) opts = parseArgs rest ({ disassemble := True } opts)
parseArgs ("-d" :: rest) opts = parseArgs rest ({ disassemble := True } opts)
parseArgs ("--gas" :: g :: rest) opts =
  let gas = fromMaybe 1000000 (parsePositive g)
  in parseArgs rest ({ gasLimit := gas } opts)
parseArgs ("--calldata" :: cd :: rest) opts =
  parseArgs rest ({ calldataHex := cd } opts)
parseArgs ("--bytecode" :: bc :: rest) opts =
  parseArgs rest ({ bytecodeHex := Just bc } opts)
parseArgs (arg :: rest) opts =
  if isPrefixOf "-" arg
    then parseArgs rest opts  -- Skip unknown flags
    else parseArgs rest ({ bytecodeFile := Just arg } opts)

-- =============================================================================
-- Help Text
-- =============================================================================

helpText : String
helpText = """
idris2-evm - Pure Idris2 EVM Interpreter

USAGE:
  idris2-evm-run [options] <bytecode-file>
  idris2-evm-run --bytecode 0x600160... [options]

DESCRIPTION:
  Executes EVM bytecode through a pure Idris2 interpreter.

  When this tool is compiled with Idris2's --coverage flag, the Chez Scheme
  profiler tracks which interpreter branches are executed. This gives
  semantic coverage of your EVM/Yul code mapped back to Idris2 functions.

OPTIONS:
  -h, --help              Show this help message
  -v, --version           Show version
  --verbose               Verbose output (show each step)
  -d, --disassemble       Disassemble bytecode and exit
  --gas <limit>           Gas limit (default: 1000000)
  --calldata <hex>        Calldata as hex (e.g., 0x371303c0)
  --bytecode <hex>        Bytecode as hex (alternative to file)

EXAMPLES:
  # Execute bytecode file with calldata
  idris2-evm-run --calldata 0x371303c0 contract.bin

  # Execute inline bytecode (Counter increment)
  idris2-evm-run --bytecode 0x60016000540160005500 --calldata 0x

  # Disassemble bytecode
  idris2-evm-run -d contract.bin

INTEGRATION WITH COVERAGE:
  To collect coverage data, rebuild with --coverage:
    idris2 --cg chez --coverage src/Main.idr -o idris2-evm-run

  Then run the interpreter. The Chez .ssi files will contain hit counts
  for each branch in the interpreter, reflecting which EVM opcodes
  were executed.

SUPPORTED OPCODES:
  - Arithmetic: ADD, MUL, SUB, DIV, MOD, EXP, etc.
  - Comparison: LT, GT, EQ, ISZERO, etc.
  - Bitwise: AND, OR, XOR, NOT, SHL, SHR, etc.
  - Memory: MLOAD, MSTORE, MSTORE8
  - Storage: SLOAD, SSTORE
  - Control: JUMP, JUMPI, JUMPDEST, STOP, RETURN, REVERT
  - Stack: POP, PUSH0-PUSH32, DUP1-DUP16, SWAP1-SWAP16
  - Environment: CALLER, CALLVALUE, CALLDATALOAD, etc.
  - Block: NUMBER, TIMESTAMP, CHAINID, etc.
"""

versionText : String
versionText = "idris2-evm 0.2.0 - Pure Idris2 EVM Interpreter"

-- =============================================================================
-- Main Execution
-- =============================================================================

loadBytecode : Options -> IO (Either String Bytecode)
loadBytecode opts =
  case opts.bytecodeHex of
    Just hex =>
      case fromHex hex of
        Just code => pure (Right code)
        Nothing => pure (Left "Invalid bytecode hex")
    Nothing =>
      case opts.bytecodeFile of
        Nothing => pure (Left "No bytecode specified")
        Just path => do
          Right content <- readFile path
            | Left err => pure (Left $ "Failed to read file: " ++ show err)
          let trimmed = trim content
          case fromHex trimmed of
            Just code => pure (Right code)
            Nothing => pure (Left "Invalid bytecode in file")

runMain : Options -> IO ()
runMain opts = do
  codeResult <- loadBytecode opts
  case codeResult of
    Left err => do
      putStrLn $ "Error: " ++ err
      exitWith (ExitFailure 1)
    Right code => do
      when opts.verbose $ do
        putStrLn $ "Bytecode size: " ++ show (codeSize code) ++ " bytes"
        putStrLn $ "Gas limit: " ++ show opts.gasLimit
        putStrLn $ "Calldata: " ++ (if null opts.calldataHex then "(none)" else opts.calldataHex)
        putStrLn ""

      if opts.disassemble
        then do
          putStrLn "=== Disassembly ==="
          putStrLn $ showDisassembly code
        else do
          let calldata = fromMaybe [] (fromHex opts.calldataHex)
          when opts.verbose $ do
            putStrLn "=== Execution ==="

          let result = execute code calldata opts.gasLimit

          case result of
            Success retData gasUsed store => do
              putStrLn $ "Result: SUCCESS"
              putStrLn $ "Gas used: " ++ show (opts.gasLimit `minus` gasUsed)
              when (not $ null retData) $ do
                putStrLn $ "Return data: " ++ toHex retData
              let slots = Storage.toList store
              when (not $ null slots) $ do
                putStrLn "Storage:"
                for_ slots $ \(k, v) =>
                  putStrLn $ "  " ++ show k ++ " => " ++ show v

            Revert retData gasUsed => do
              putStrLn $ "Result: REVERT"
              putStrLn $ "Gas used: " ++ show (opts.gasLimit `minus` gasUsed)
              when (not $ null retData) $ do
                putStrLn $ "Revert data: " ++ toHex retData

            OutOfGas => do
              putStrLn "Result: OUT OF GAS"

            InvalidJump pc => do
              putStrLn $ "Result: INVALID JUMP to PC=" ++ show pc

            StackError msg => do
              putStrLn $ "Result: STACK ERROR - " ++ msg

            InvalidOpcode op => do
              putStrLn $ "Result: INVALID OPCODE 0x" ++ show op

-- =============================================================================
-- Entry Point
-- =============================================================================

main : IO ()
main = do
  args <- getArgs
  let opts = parseArgs (drop 1 args) defaultOptions

  if opts.showHelp
    then putStrLn helpText
    else if opts.showVersion
      then putStrLn versionText
      else runMain opts
