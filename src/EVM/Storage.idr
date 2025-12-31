||| EVM.Storage - EVM Storage model
|||
||| EVM storage is a persistent key-value store.
||| Both keys and values are 256-bit words.
module EVM.Storage

import EVM.Word256
import Data.Maybe
import Data.List
import Data.String
import Data.SortedMap

%default covering

||| Storage key type (256-bit)
public export
StorageKey : Type
StorageKey = Word256

||| Storage value type (256-bit)
public export
StorageValue : Type
StorageValue = Word256

||| EVM Storage - persistent key-value mapping
public export
record Storage where
  constructor MkStorage
  slots : SortedMap Integer Word256  -- key as Integer for SortedMap

||| Empty storage
public export
empty : Storage
empty = MkStorage empty

||| SLOAD - load from storage
public export
sload : StorageKey -> Storage -> StorageValue
sload key store =
  let k = toInteger key
  in fromMaybe Word256.zero (lookup k store.slots)

||| SSTORE - store to storage
public export
sstore : StorageKey -> StorageValue -> Storage -> Storage
sstore key val store =
  let k = toInteger key
  in MkStorage (insert k val store.slots)

||| Check if slot has been accessed (for gas calculation)
public export
isWarm : StorageKey -> Storage -> Bool
isWarm key store =
  isJust (lookup (toInteger key) store.slots)

||| Get all storage slots (for debugging/introspection)
public export
toList : Storage -> List (Word256, Word256)
toList store =
  map (\(k, v) => (fromInteger k, v)) (SortedMap.toList store.slots)

||| Get number of non-zero slots
public export
nonZeroCount : Storage -> Nat
nonZeroCount store =
  length $ filter (\(_, v) => v /= Word256.zero) (SortedMap.toList store.slots)

||| Show instance for debugging
public export
Show Storage where
  show store =
    let slots = SortedMap.toList store.slots
        showSlot : (Integer, Word256) -> String
        showSlot (k, v) = "  " ++ show (Word256.fromInteger k) ++ " => " ++ show v
    in "Storage{\n" ++ unlines (map showSlot slots) ++ "}"
