||| EVM.Interpreter - Pure Idris2 EVM Interpreter
|||
||| This is the core EVM execution engine, written in pure Idris2.
||| It can be compiled with --coverage to generate Chez Scheme profiler data.
|||
||| Key insight: By running EVM bytecode through pure Idris2 functions,
||| the Chez profiler tracks which interpreter branches are executed,
||| giving us semantic coverage of the EVM code.
module EVM.Interpreter

import EVM.Word256
import EVM.Stack
import EVM.Memory
import EVM.Storage
import EVM.Opcodes
import EVM.Bytecode
import Data.List

%default covering

-- =============================================================================
-- EVM State
-- =============================================================================

||| Execution context - external inputs
public export
record Context where
  constructor MkContext
  address : Word256      -- Contract address
  caller : Word256       -- Message sender
  origin : Word256       -- Transaction origin
  callValue : Word256    -- Wei sent
  callData : List Bits8  -- Input data
  gasPrice : Word256     -- Gas price
  blockNumber : Word256  -- Block number
  timestamp : Word256    -- Block timestamp
  chainId : Word256      -- Chain ID
  gasLimit : Word256     -- Block gas limit

||| Default context for testing
public export
defaultContext : Context
defaultContext = MkContext
  (fromInteger 0x1234567890ABCDEF)  -- address
  (fromInteger 0xCAFEBABE)          -- caller
  (fromInteger 0xCAFEBABE)          -- origin
  Word256.zero                      -- callValue
  []                                -- callData
  (fromInteger 1)                   -- gasPrice
  (fromInteger 1000000)             -- blockNumber
  (fromInteger 1700000000)          -- timestamp
  (fromInteger 1)                   -- chainId
  (fromInteger 30000000)            -- gasLimit

||| EVM Machine State
public export
record VM where
  constructor MkVM
  pc : Nat               -- Program counter
  stack : Stack          -- Stack
  memory : Memory        -- Memory
  storage : Storage      -- Storage
  code : Bytecode        -- Bytecode being executed
  gas : Nat              -- Remaining gas
  context : Context      -- Execution context
  returnData : List Bits8 -- Return data from last call
  stopped : Bool         -- Execution stopped?
  reverted : Bool        -- Execution reverted?

||| Create initial VM state
public export
initVM : Bytecode -> Context -> Nat -> VM
initVM code ctx gas = MkVM 0 Stack.empty Memory.empty Storage.empty
                          code gas ctx [] False False

||| Execution result
public export
data Result : Type where
  Success : (returnData : List Bits8) -> (gasUsed : Nat) -> (storage : Storage) -> Result
  Revert : (returnData : List Bits8) -> (gasUsed : Nat) -> Result
  OutOfGas : Result
  InvalidJump : (pc : Nat) -> Result
  StackError : (msg : String) -> Result
  InvalidOpcode : (op : Bits8) -> Result

public export
Show Result where
  show (Success ret gas st) = "Success(gas=" ++ show gas ++ ", return=" ++ show (length ret) ++ " bytes)"
  show (Revert ret gas) = "Revert(gas=" ++ show gas ++ ", return=" ++ show (length ret) ++ " bytes)"
  show OutOfGas = "OutOfGas"
  show (InvalidJump pc) = "InvalidJump(pc=" ++ show pc ++ ")"
  show (StackError msg) = "StackError(" ++ msg ++ ")"
  show (InvalidOpcode op) = "InvalidOpcode(0x" ++ show op ++ ")"

-- =============================================================================
-- Helper Operations (defined before use)
-- =============================================================================

||| Push a value onto stack
pushVal : Word256 -> VM -> Either Result VM
pushVal val vm =
  case push val vm.stack of
    Ok stack' => Right ({ stack := stack', pc $= (+1) } vm)
    _ => Left (StackError "push overflow")

||| Unary stack operation
unaryOp : (Word256 -> Word256) -> VM -> Either Result VM
unaryOp f vm =
  case pop vm.stack of
    Ok (a, stack') =>
      case push (f a) stack' of
        Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
        _ => Left (StackError "unary push failed")
    _ => Left (StackError "unary pop failed")

||| Binary stack operation
binaryOp : (Word256 -> Word256 -> Word256) -> VM -> Either Result VM
binaryOp f vm =
  case popN 2 vm.stack of
    Ok ([a, b], stack') =>
      case push (f a b) stack' of
        Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
        _ => Left (StackError "binary push failed")
    _ => Left (StackError "binary pop failed")

||| Ternary stack operation
ternaryOp : (Word256 -> Word256 -> Word256 -> Word256) -> VM -> Either Result VM
ternaryOp f vm =
  case popN 3 vm.stack of
    Ok ([a, b, c], stack') =>
      case push (f a b c) stack' of
        Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
        _ => Left (StackError "ternary push failed")
    _ => Left (StackError "ternary pop failed")

||| DUP operation
dupOp : Nat -> VM -> Either Result VM
dupOp n vm =
  case dup n vm.stack of
    Ok stack' => Right ({ stack := stack', pc $= (+1) } vm)
    _ => Left (StackError $ "DUP" ++ show (n + 1) ++ " failed")

||| SWAP operation
swapOp : Nat -> VM -> Either Result VM
swapOp n vm =
  case swap n vm.stack of
    Ok stack' => Right ({ stack := stack', pc $= (+1) } vm)
    _ => Left (StackError $ "SWAP" ++ show n ++ " failed")

||| LOG operation (discards data, advances PC)
logOp : Nat -> VM -> Either Result VM
logOp numTopics vm =
  case popN (2 + numTopics) vm.stack of
    Ok (_, stack') => Right ({ stack := stack', pc $= (+1) } vm)
    _ => Left (StackError $ "LOG" ++ show numTopics ++ " failed")

toByteListFromMem : Memory -> List Bits8
toByteListFromMem mem = map (\i => readByte i mem) [0 .. cast (memorySize mem * 32)]

||| RETURN operation
returnOp : VM -> Either Result VM
returnOp vm =
  case popN 2 vm.stack of
    Ok ([offset, size], _) =>
      let retData = readBytes (cast $ toInteger offset) (cast $ toInteger size) (toByteListFromMem vm.memory)
      in Right ({ stopped := True, returnData := retData } vm)
    _ => Left (StackError "RETURN failed")

||| REVERT operation
revertOp : VM -> Either Result VM
revertOp vm =
  case popN 2 vm.stack of
    Ok ([offset, size], _) =>
      let retData = readBytes (cast $ toInteger offset) (cast $ toInteger size) (toByteListFromMem vm.memory)
      in Right ({ stopped := True, reverted := True, returnData := retData } vm)
    _ => Left (StackError "REVERT failed")

||| CREATE stub
createStub : VM -> Either Result VM
createStub vm =
  case popN 3 vm.stack of
    Ok (_, stack') =>
      case push Word256.zero stack' of  -- Return 0 (failed create)
        Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
        _ => Left (StackError "CREATE push failed")
    _ => Left (StackError "CREATE failed")

||| CALL stub
callStub : VM -> Either Result VM
callStub vm =
  case popN 7 vm.stack of
    Ok (_, stack') =>
      case push Word256.one stack' of  -- Return 1 (success)
        Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
        _ => Left (StackError "CALL push failed")
    _ =>
      case popN 6 vm.stack of  -- STATICCALL/DELEGATECALL have 6 args
        Ok (_, stack') =>
          case push Word256.one stack' of
            Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
            _ => Left (StackError "CALL push failed")
        _ => Left (StackError "CALL failed")

-- =============================================================================
-- Step Execution - Core Interpreter
-- =============================================================================

||| Read push data from bytecode
readPushData : Nat -> Nat -> Bytecode -> Word256
readPushData pc size code =
  let bytes = readBytes (pc + 1) size code
      val = foldl (\acc, b => acc * 256 + cast b) 0 bytes
  in fromInteger val

||| Power function helper
pow : Integer -> Nat -> Integer
pow _ 0 = 1
pow b (S n) = b * pow b n

mutual
  ||| Execute a single step
  ||| This is the key function for coverage - each opcode branch is tracked
  export
  partial
  step : VM -> Either Result VM
  step vm =
    if vm.stopped then Left (Success vm.returnData (vm.gas) vm.storage)
    else if vm.reverted then Left (Revert vm.returnData (vm.gas))
    else if vm.pc >= codeSize vm.code then Left (Success [] vm.gas vm.storage)
    else
      let opByte = readAt vm.pc vm.code
          op = fromByte opByte
      in executeOp op vm

  ||| Execute an opcode
  ||| Each case is a distinct branch for coverage tracking
  partial
  executeOp : Opcode -> VM -> Either Result VM
  executeOp STOP vm =
    Right ({ stopped := True } vm)

  -- Arithmetic
  executeOp ADD vm = binaryOp Word256.add vm
  executeOp MUL vm = binaryOp Word256.mul vm
  executeOp SUB vm = binaryOp Word256.sub vm
  executeOp DIV vm = binaryOp Word256.div vm
  executeOp SDIV vm = binaryOp Word256.sdiv vm
  executeOp MOD vm = binaryOp Word256.mod vm
  executeOp SMOD vm = binaryOp Word256.mod vm  -- Simplified
  executeOp ADDMOD vm = ternaryOp addmod vm where
    addmod : Word256 -> Word256 -> Word256 -> Word256
    addmod a b n = if n == Word256.zero then Word256.zero
                   else fromInteger ((toInteger a + toInteger b) `mod` toInteger n)
  executeOp MULMOD vm = ternaryOp mulmod vm where
    mulmod : Word256 -> Word256 -> Word256 -> Word256
    mulmod a b n = if n == Word256.zero then Word256.zero
                   else fromInteger ((toInteger a * toInteger b) `mod` toInteger n)
  executeOp EXP vm = binaryOp expOp vm where
    expOp : Word256 -> Word256 -> Word256
    expOp base exponent = fromInteger (pow (toInteger base) (cast $ toInteger exponent))
  executeOp SIGNEXTEND vm = binaryOp (\_, x => x) vm  -- Simplified

  -- Comparison
  executeOp LT vm = binaryOp Word256.lt vm
  executeOp GT vm = binaryOp Word256.gt vm
  executeOp SLT vm = binaryOp Word256.lt vm  -- Simplified (should be signed)
  executeOp SGT vm = binaryOp Word256.gt vm  -- Simplified (should be signed)
  executeOp EQ vm = binaryOp Word256.eq vm
  executeOp ISZERO vm = unaryOp Word256.iszero vm

  -- Bitwise
  executeOp AND vm = binaryOp Word256.and vm
  executeOp OR vm = binaryOp Word256.or vm
  executeOp XOR vm = binaryOp Word256.xor vm
  executeOp NOT vm = unaryOp Word256.not vm
  executeOp BYTE vm = binaryOp Word256.byte vm
  executeOp SHL vm = binaryOp Word256.shl vm
  executeOp SHR vm = binaryOp Word256.shr vm
  executeOp SAR vm = binaryOp Word256.shr vm  -- Simplified (should be arithmetic)

  -- Keccak256 (stubbed - returns hash of length)
  executeOp KECCAK256 vm =
    case popN 2 vm.stack of
      Underflow => Left (StackError "KECCAK256 underflow")
      Overflow => Left (StackError "KECCAK256 overflow")
      Ok ([offset, size], stack') =>
        -- Stub: return a deterministic "hash" based on offset and size
        let hash = fromInteger (toInteger offset + toInteger size * 256)
        in case push hash stack' of
             Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
             _ => Left (StackError "KECCAK256 push failed")
      Ok (_, _) => Left (StackError "KECCAK256 invalid")

  -- Environmental
  executeOp ADDRESS vm = pushVal vm.context.address vm
  executeOp BALANCE vm = unaryOp (\_ => fromInteger 1000000000000000000) vm  -- Stub: 1 ETH
  executeOp ORIGIN vm = pushVal vm.context.origin vm
  executeOp CALLER vm = pushVal vm.context.caller vm
  executeOp CALLVALUE vm = pushVal vm.context.callValue vm
  executeOp CALLDATALOAD vm =
    case pop vm.stack of
      Underflow => Left (StackError "CALLDATALOAD underflow")
      Ok (offset, stack') =>
        let idx = cast {to=Nat} (toInteger offset)
            bytes = take 32 (drop idx vm.context.callData ++ replicate 32 0)
            val = foldl (\acc, b => acc * 256 + cast b) 0 bytes
        in case push (fromInteger val) stack' of
             Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
             _ => Left (StackError "CALLDATALOAD push failed")
  executeOp CALLDATASIZE vm = pushVal (fromInteger $ cast $ length vm.context.callData) vm
  executeOp CALLDATACOPY vm =
    case popN 3 vm.stack of
      Ok ([destOffset, dataOffset, size], stack') =>
        let mem' = calldatacopy (toInteger destOffset)
                                (cast $ toInteger dataOffset)
                                (cast $ toInteger size)
                                vm.context.callData
                                vm.memory
        in Right ({ stack := stack', memory := mem', pc $= (+1) } vm)
      _ => Left (StackError "CALLDATACOPY failed")
  executeOp CODESIZE vm = pushVal (fromInteger $ cast $ codeSize vm.code) vm
  executeOp CODECOPY vm =
    case popN 3 vm.stack of
      Ok ([destOffset, codeOffset, size], stack') =>
        let codeBytes = readBytes (cast $ toInteger codeOffset) (cast $ toInteger size) vm.code
            mem' = writeBytes (toInteger destOffset) codeBytes vm.memory
        in Right ({ stack := stack', memory := mem', pc $= (+1) } vm)
      _ => Left (StackError "CODECOPY failed")
  executeOp GASPRICE vm = pushVal vm.context.gasPrice vm
  executeOp EXTCODESIZE vm = unaryOp (\_ => Word256.zero) vm  -- Stub
  executeOp EXTCODECOPY vm = case popN 4 vm.stack of
    Ok (_, stack') => Right ({ stack := stack', pc $= (+1) } vm)
    _ => Left (StackError "EXTCODECOPY failed")
  executeOp RETURNDATASIZE vm = pushVal (fromInteger $ cast $ length vm.returnData) vm
  executeOp RETURNDATACOPY vm =
    case popN 3 vm.stack of
      Ok ([destOffset, dataOffset, size], stack') =>
        let dataBytes = take (cast $ toInteger size) (drop (cast $ toInteger dataOffset) vm.returnData)
            mem' = writeBytes (toInteger destOffset) dataBytes vm.memory
        in Right ({ stack := stack', memory := mem', pc $= (+1) } vm)
      _ => Left (StackError "RETURNDATACOPY failed")
  executeOp EXTCODEHASH vm = unaryOp (\_ => Word256.zero) vm  -- Stub

  -- Block info
  executeOp BLOCKHASH vm = unaryOp (\_ => Word256.zero) vm  -- Stub
  executeOp COINBASE vm = pushVal Word256.zero vm  -- Stub
  executeOp TIMESTAMP vm = pushVal vm.context.timestamp vm
  executeOp NUMBER vm = pushVal vm.context.blockNumber vm
  executeOp PREVRANDAO vm = pushVal Word256.zero vm  -- Stub
  executeOp GASLIMIT vm = pushVal vm.context.gasLimit vm
  executeOp CHAINID vm = pushVal vm.context.chainId vm
  executeOp SELFBALANCE vm = pushVal (fromInteger 1000000000000000000) vm  -- Stub: 1 ETH
  executeOp BASEFEE vm = pushVal Word256.one vm  -- Stub
  executeOp BLOBHASH vm = unaryOp (\_ => Word256.zero) vm  -- Stub
  executeOp BLOBBASEFEE vm = pushVal Word256.one vm  -- Stub

  -- Stack, Memory, Storage
  executeOp POP vm =
    case pop vm.stack of
      Ok (_, stack') => Right ({ stack := stack', pc $= (+1) } vm)
      Underflow => Left (StackError "POP underflow")
      Overflow => Left (StackError "POP overflow")

  executeOp MLOAD vm =
    case pop vm.stack of
      Ok (offset, stack') =>
        let val = mload offset vm.memory
        in case push val stack' of
             Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
             _ => Left (StackError "MLOAD push failed")
      _ => Left (StackError "MLOAD failed")

  executeOp MSTORE vm =
    case popN 2 vm.stack of
      Ok ([offset, val], stack') =>
        let mem' = mstore offset val vm.memory
        in Right ({ stack := stack', memory := mem', pc $= (+1) } vm)
      _ => Left (StackError "MSTORE failed")

  executeOp MSTORE8 vm =
    case popN 2 vm.stack of
      Ok ([offset, val], stack') =>
        let mem' = mstore8 offset val vm.memory
        in Right ({ stack := stack', memory := mem', pc $= (+1) } vm)
      _ => Left (StackError "MSTORE8 failed")

  executeOp SLOAD vm =
    case pop vm.stack of
      Ok (key, stack') =>
        let val = sload key vm.storage
        in case push val stack' of
             Ok stack'' => Right ({ stack := stack'', pc $= (+1) } vm)
             _ => Left (StackError "SLOAD push failed")
      _ => Left (StackError "SLOAD failed")

  executeOp SSTORE vm =
    case popN 2 vm.stack of
      Ok ([key, val], stack') =>
        let store' = sstore key val vm.storage
        in Right ({ stack := stack', storage := store', pc $= (+1) } vm)
      _ => Left (StackError "SSTORE failed")

  -- Control flow
  executeOp JUMP vm =
    case pop vm.stack of
      Ok (dest, stack') =>
        let destNat = cast {to=Nat} (toInteger dest)
        in if isValidJumpDest destNat vm.code
             then Right ({ stack := stack', pc := destNat } vm)
             else Left (InvalidJump destNat)
      _ => Left (StackError "JUMP failed")

  executeOp JUMPI vm =
    case popN 2 vm.stack of
      Ok ([dest, cond], stack') =>
        if cond /= Word256.zero
          then let destNat = cast {to=Nat} (toInteger dest)
               in if isValidJumpDest destNat vm.code
                    then Right ({ stack := stack', pc := destNat } vm)
                    else Left (InvalidJump destNat)
          else Right ({ stack := stack', pc $= (+1) } vm)
      _ => Left (StackError "JUMPI failed")

  executeOp PC vm = pushVal (fromInteger $ cast vm.pc) vm
  executeOp MSIZE vm = pushVal (fromInteger $ cast $ memorySize vm.memory * 32) vm
  executeOp GAS vm = pushVal (fromInteger $ cast vm.gas) vm
  executeOp JUMPDEST vm = Right ({ pc $= (+1) } vm)  -- No-op marker

  -- Transient storage (EIP-1153)
  executeOp TLOAD vm = executeOp SLOAD vm   -- Simplified: treat as SLOAD
  executeOp TSTORE vm = executeOp SSTORE vm -- Simplified: treat as SSTORE
  executeOp MCOPY vm =
    case popN 3 vm.stack of
      Ok ([dest, src, size], stack') =>
        let mem' = mcopy (toInteger dest) (toInteger src) (cast $ toInteger size) vm.memory
        in Right ({ stack := stack', memory := mem', pc $= (+1) } vm)
      _ => Left (StackError "MCOPY failed")

  -- Push operations
  executeOp PUSH0 vm =
    case push Word256.zero vm.stack of
      Ok stack' => Right ({ stack := stack', pc $= (+1) } vm)
      _ => Left (StackError "PUSH0 overflow")

  executeOp op vm =
    if isPush op
      then let size = pushSize op
               val = readPushData vm.pc size vm.code
           in case push val vm.stack of
                Ok stack' => Right ({ stack := stack', pc $= (+ (1 + size)) } vm)
                _ => Left (StackError "PUSH overflow")
      else executeDupSwap op vm

  ||| Handle DUP and SWAP
  partial
  executeDupSwap : Opcode -> VM -> Either Result VM
  executeDupSwap DUP1 vm = dupOp 0 vm
  executeDupSwap DUP2 vm = dupOp 1 vm
  executeDupSwap DUP3 vm = dupOp 2 vm
  executeDupSwap DUP4 vm = dupOp 3 vm
  executeDupSwap DUP5 vm = dupOp 4 vm
  executeDupSwap DUP6 vm = dupOp 5 vm
  executeDupSwap DUP7 vm = dupOp 6 vm
  executeDupSwap DUP8 vm = dupOp 7 vm
  executeDupSwap DUP9 vm = dupOp 8 vm
  executeDupSwap DUP10 vm = dupOp 9 vm
  executeDupSwap DUP11 vm = dupOp 10 vm
  executeDupSwap DUP12 vm = dupOp 11 vm
  executeDupSwap DUP13 vm = dupOp 12 vm
  executeDupSwap DUP14 vm = dupOp 13 vm
  executeDupSwap DUP15 vm = dupOp 14 vm
  executeDupSwap DUP16 vm = dupOp 15 vm
  executeDupSwap SWAP1 vm = swapOp 1 vm
  executeDupSwap SWAP2 vm = swapOp 2 vm
  executeDupSwap SWAP3 vm = swapOp 3 vm
  executeDupSwap SWAP4 vm = swapOp 4 vm
  executeDupSwap SWAP5 vm = swapOp 5 vm
  executeDupSwap SWAP6 vm = swapOp 6 vm
  executeDupSwap SWAP7 vm = swapOp 7 vm
  executeDupSwap SWAP8 vm = swapOp 8 vm
  executeDupSwap SWAP9 vm = swapOp 9 vm
  executeDupSwap SWAP10 vm = swapOp 10 vm
  executeDupSwap SWAP11 vm = swapOp 11 vm
  executeDupSwap SWAP12 vm = swapOp 12 vm
  executeDupSwap SWAP13 vm = swapOp 13 vm
  executeDupSwap SWAP14 vm = swapOp 14 vm
  executeDupSwap SWAP15 vm = swapOp 15 vm
  executeDupSwap SWAP16 vm = swapOp 16 vm
  executeDupSwap LOG0 vm = logOp 0 vm
  executeDupSwap LOG1 vm = logOp 1 vm
  executeDupSwap LOG2 vm = logOp 2 vm
  executeDupSwap LOG3 vm = logOp 3 vm
  executeDupSwap LOG4 vm = logOp 4 vm
  executeDupSwap RETURN vm = returnOp vm
  executeDupSwap REVERT vm = revertOp vm
  executeDupSwap INVALID vm = Left (InvalidOpcode 0xfe)
  executeDupSwap SELFDESTRUCT vm = Right ({ stopped := True } vm)
  executeDupSwap CREATE vm = createStub vm
  executeDupSwap CREATE2 vm = createStub vm
  executeDupSwap CALL vm = callStub vm
  executeDupSwap CALLCODE vm = callStub vm
  executeDupSwap DELEGATECALL vm = callStub vm
  executeDupSwap STATICCALL vm = callStub vm
  executeDupSwap (UNKNOWN b) vm = Left (InvalidOpcode b)
  executeDupSwap _ vm = Right ({ pc $= (+1) } vm)  -- Catch-all

-- =============================================================================
-- Main Execution
-- =============================================================================

||| Run until completion (with step limit for safety)
export
partial
run : Nat -> VM -> Result
run 0 vm = OutOfGas  -- Step limit reached
run (S n) vm =
  case step vm of
    Left result => result
    Right vm' => run n vm'

||| Execute bytecode with calldata
export
partial
execute : Bytecode -> List Bits8 -> Nat -> Result
execute code calldata gasLimit =
  let ctx = { callData := calldata } defaultContext
      vm = initVM code ctx gasLimit
  in run gasLimit vm

||| Execute bytecode from hex string
export
partial
executeHex : String -> String -> Nat -> Either String Result
executeHex codeHex calldataHex gasLimit =
  case (EVM.Bytecode.fromHex codeHex, EVM.Bytecode.fromHex calldataHex) of
    (Just code, Just calldata) => Right $ execute code calldata gasLimit
    (Nothing, _) => Left "Invalid bytecode hex"
    (_, Nothing) => Left "Invalid calldata hex"
