{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies #-}
module VM
  ( Bytes
  , fromBytes
  , VM
  , runPrivileged
  , privileged
  , fail
  , checkFinal
  , finish
  , putIntcblock
  , intcblock
  , putBytecblock
  , bytecblock
  , mode
  , logicSigVersion
  , push
  , pop
  , add
  , sub
  , div
  , mul
  , mod
  , eq
  , lt
  , isZero
  , jump
  , global
  , transaction
  , transactionArray
  , groupTransaction
  , groupTransactionArray
  , store
  , load
  , balance
  , minBalance
  , appGlobalGet
  , appGlobalPut
  , appGlobalGetEx
  , appLocalGet
  , appLocalPut
  , appLocalDel
  , assetParamsGet
  , assetHoldingGet
  , keccak256
  , itob
  , btoi
  , substring
  , substring3
  , concat
  , getbyte
  , log
  , execute
  , continue
  , logicSigVersionGE
  )
where

import Prelude hiding (concat, div, fail, log, mod)
import Numeric (showHex)
import Mode
import ReadByte
import Data.Word (Word8)

class Bytes a where
  fromBytes :: [Word8] -> a

class (ReadByte m, Num s, Bytes s) => VM m s | m -> s where
  runPrivileged :: m a -> m a
  privileged :: m Bool
  fail :: String -> m a
  checkFinal :: m ()
  finish :: s -> m a
  putIntcblock :: [Integer] -> m ()
  intcblock :: Integer -> m s
  putBytecblock :: [[Word8]] -> m ()
  bytecblock :: Integer -> m s
  mode :: m Mode
  logicSigVersion :: m Integer
  push :: s -> m ()
  pop :: m s
  add :: s -> s -> m s
  sub :: s -> s -> m s
  div :: s -> s -> m s
  mul :: s -> s -> m s
  mod :: s -> s -> m s
  eq :: s -> s -> m Bool
  lt :: s -> s -> m Bool
  isZero :: s -> m Bool
  jump :: Integer -> m ()
  global :: Integer -> m s
  transaction :: Integer -> m s
  transactionArray :: Integer -> Integer -> m s
  groupTransaction :: Integer -> Integer -> m s
  groupTransactionArray :: Integer -> Integer -> Integer -> m s
  store :: Integer -> s -> m ()
  load :: Integer -> m s
  balance :: s -> m s
  minBalance :: s -> m s
  appGlobalGet :: s -> m s
  appGlobalPut :: s -> s -> m ()
  appGlobalGetEx :: s -> s -> m (Maybe s)
  appLocalGet :: s -> s -> m s
  appLocalPut :: s -> s -> s -> m ()
  appLocalDel :: s -> s -> m ()
  assetParamsGet :: s -> Integer -> m (Maybe s)
  assetHoldingGet :: s -> s -> Integer -> m (Maybe s)
  keccak256 :: s -> m s
  itob :: s -> m s
  btoi :: s -> m s
  substring :: Integer -> Integer -> s -> m s
  substring3 :: s -> s -> s -> m s
  concat :: s -> s -> m s
  getbyte :: s -> s -> m s
  log :: s -> m ()
  -- attempt MaxLogCalls (16) + 1 fails and aborts execution
  -- attempt to log MaxLogSize (1000) + 1 byte fails and aborts execution
  -- perhaps write log calls to ledger

{-
an instance of VM should:
- respect the limitations of each instruction
  e.g., byte-arithmetic operations take at most 64 bytes per operand
- ensure that more global limitations are enforced:
  - max stack size (1000)
  - max execution cost (depends on mode)
  - max number of log calls
  - max amount of logged data
-}
  
unused :: VM m s => Integer -> m a
unused oc = fail $ "XXX use of unused opcode: 0x" ++ (showHex oc "")

stub :: VM m s => String -> m a
stub instr = fail $ "XXX finish implementation for " ++ instr

continue :: VM m s => m ()
continue = return ()

logicSigVersionGE :: VM m s => Integer -> String -> m ()
logicSigVersionGE lsv part = do
  priv <- privileged
  case priv of
    True -> continue
    False -> do
      actualLsv <- logicSigVersion
      if | lsv > actualLsv -> fail $ "need LogicSigVersion >= " ++ (show lsv) ++ " for " ++ part ++ " but LogicSigVersion = " ++ (show actualLsv)
         | otherwise -> continue

inMode :: VM m s => Mode -> String -> m ()
inMode md part = do
  actualMd <- mode
  if actualMd /= md
    then fail $ "need mode " ++ (show md) ++ " for " ++ part ++ " but in mode " ++ (show actualMd)
    else continue

cost :: (Monad m) => Integer -> m ()
cost _ = return ()

{-

callsub offset = ThisVM $ \s -> Partial s 42
retsub = ThisVM $ \s -> Partial s 42

push x = ThisVM $ \s -> Partial s 42
pop = ThisVM $ \s -> Partial s 42

pop_uint64 = do
  x <- pop
  return x
-}

execute :: VM m s => Integer -> m ()
execute 0x00 = fail "err"
execute 0x01 = stub "sha256"
execute 0x02 = do -- keccak
  lsv <- logicSigVersion
  cost $ if lsv == 1 then 26 else 130
  (pop >>= keccak256) >>= push
  
execute 0x03 = stub "sha512_256"
execute 0x04 = stub "ed25519verify"
-- 0x05, 0x06, 0x07 unused
execute 0x08 = do -- +
  b <- pop
  a <- pop
  add a b >>= push
execute 0x09 = do -- -
  b <- pop
  a <- pop
  sub a b >>= push
execute 0x0a = do -- /
  b <- pop
  a <- pop
  div a b >>= push
execute 0x0b = do -- *
  b <- pop
  a <- pop
  mul a b >>= push
execute 0x0c = do -- <
  b <- pop
  a <- pop
  huh <- lt a b
  push $ if huh then 1 else 0
-- the following are implemented as the Go VM
-- but we may need to treat each specially
-- for precision purposes
execute 0x0d = runPrivileged $ do -- >
  execute 0x4c -- swap
  execute 0x0c -- <
execute 0x0e = runPrivileged $ do -- <=
  execute 0x0d -- >
  execute 0x14 -- !
execute 0x0f = runPrivileged $ do -- >=
  execute 0x0c -- <
  execute 0x14 -- !
execute 0x10 = do -- &&
  b <- pop
  a <- pop
  huhb <- isZero b
  huha <- isZero a
  push $ if huha || huhb then 0 else 1
execute 0x11 = do -- ||
  b <- pop
  a <- pop
  huhb <- isZero b
  huha <- isZero a
  push $ if huha && huhb then 0 else 1
execute 0x12 = do -- ==
  b <- pop
  a <- pop
  huh <- eq a b
  push $ if huh then 1 else 0
execute 0x13 = runPrivileged $ do -- !=
  execute 0x12 -- ==
  execute 0x14 -- !
execute 0x14 = do -- !
  a <- pop
  huh <- isZero a
  push $ if huh then 1 else 0
execute 0x15 = stub "len"
execute 0x16 = (pop >>= itob) >>= push -- itob
execute 0x17 = (pop >>= btoi) >>= push -- btoi
execute 0x18 = do -- %
  b <- pop
  a <- pop
  mod a b >>= push
execute 0x19 = stub "|"
execute 0x1a = stub "&"
execute 0x1b = stub "^"
execute 0x1c = stub "~"
execute 0x1d = stub "mulw"
execute 0x1e = stub "addw"
execute 0x1f = stub "divmodw"
execute 0x20 = readIntcblock >>= putIntcblock -- intcblock
execute 0x21 = (readUint8 >>= intcblock) >>= push -- intc
execute 0x22 = (intcblock 0) >>= push -- intc_0
execute 0x23 = (intcblock 1) >>= push -- intc_1
execute 0x24 = (intcblock 2) >>= push -- intc_2
execute 0x25 = (intcblock 3) >>= push -- intc_3
execute 0x26 = readBytecblock >>= putBytecblock -- bytecblock
execute 0x27 = (readUint8 >>= bytecblock) >>= push -- bytec
execute 0x28 = (bytecblock 0) >>= push -- bytec_0
execute 0x29 = (bytecblock 1) >>= push -- bytec_1
execute 0x2a = (bytecblock 2) >>= push -- bytec_2
execute 0x2b = (bytecblock 3) >>= push -- bytec_3
{-
execute 0x2c = (readUint8 >>= argument) >>= push -- arg
execute 0x2d = (argument 0) >>= push -- arg_0
execute 0x2e = (argument 1) >>= push -- arg_1
execute 0x2f = (argument 2) >>= push -- arg_2
execute 0x30 = (argument 3) >>= push -- arg_3
-}
execute 0x31 = (readUint8 >>= transaction) >>= push -- txn
execute 0x32 = (readUint8 >>= global) >>= push -- global
execute 0x33 = do -- gtxn
  gi <- readUint8
  fi <- readUint8
  (groupTransaction gi fi) >>= push
execute 0x34 = (readUint8 >>= load) >>= push -- load
execute 0x35 = do -- store
  i <- readUint8
  x <- pop
  (store i x)
execute 0x36 = do -- txna
  logicSigVersionGE 2 "txna"
  fi <- readUint8
  fai <- readUint8
  (transactionArray fi fai) >>= push
execute 0x37 = do -- gtxna
  logicSigVersionGE 2 "gtxna"
  gi <- readUint8
  fi <- readUint8
  fai <- readUint8
  (groupTransactionArray gi fi fai) >>= push
{-
execute 0x38 = do -- gtxns
  logicSigVersionGE 3 "gtxns"
  fi <- readUint8
  gi <- pop
  (group_transaction gi fi) >>= push
execute 0x39 = do -- gtxnsa
  logicSigVersionGE 3 "gtxnsa"
  fi <- readUint8
  fai <- readUint8
  gi <- pop
  (group_transaction_array gi fi fai) >>= push
execute 0x3a = do -- gload
  -- "fails unless the requested transaction is an ApplicationCall and X < GroupIndex"
  logicSigVersionGE 4 "gload"
  inMode Application
  gi <- readUint8
  i <- readUint8
  (group_load gi i) >>= push
execute 0x3b = do -- gloads
  -- "fails unless the requested transaction is an ApplicationCall and T < GroupIndex"
  logicSigVersionGE 4 "gloads"
  inMode Application
  i <- readUint8
  gi <- pop
  (group_load gi i) >>= push
execute 0x3c = stub "gaid"
execute 0x3d = do -- gaids
  logicSigVersionGE 4 "gaids"
  inMode Application
  stub "gaids"
-- 0x3e, 0x3f unused
-}
execute 0x40 = do -- bnz
  offset <- readInt16
  huh <- pop >>= isZero
  if not huh then jump offset else continue
execute 0x41 = do -- bz
  offset <- readInt16
  huh <- pop >>= isZero
  if huh then jump offset else continue
execute 0x42 = do -- b
  logicSigVersionGE 2 "b"
  readInt16 >>= jump
execute 0x43 = do -- return
  logicSigVersionGE 2 "return"
  pop >>= finish
execute 0x44 = do -- assert
  logicSigVersionGE 3 "assert"
  huh <- pop >>= isZero
  if huh then fail "assert zero" else continue
-- 0x45, 0x46, 0x47
execute 0x48 = pop >> continue -- pop
execute 0x49 = do -- dup
  x <- pop
  push x
  push x
execute 0x4a = do -- dup2
  b <- pop
  a <- pop
  push a
  push b
  push a
  push b
execute 0x4b = do -- dig
  logicSigVersionGE 3 "dig"
  (readUint8 >>= loop) >>= push
    where loop n =
            do
              x <- pop
              if n == 0
                then
                do
                  push x
                  return x
                else
                do
                  y <- loop $ n - 1
                  push x
                  return y
execute 0x4c = do -- swap
  logicSigVersionGE 3 "swap"
  b <- pop
  a <- pop
  push b
  push a
execute 0x4d = do -- select
  logicSigVersionGE 3 "select"
  huh <- (pop >>= isZero)
  b <- pop
  a <- pop
  push $ if huh then a else b
execute 0x50 = do -- concat
  logicSigVersionGE 2 "concat"
  b <- pop
  a <- pop
  concat a b >>= push
execute 0x51 = do -- substring
  logicSigVersionGE 2 "substring"
  start <- readUint8
  end <- readUint8
  (pop >>= substring start end) >>= push
execute 0x52 = do -- substring3
  logicSigVersionGE 2 "substring"
  end <- pop
  start <- pop
  (pop >>= substring3 start end) >>= push
{-
execute 0x53 = stub "getbit"
execute 0x54 = stub "setbit"
-}
execute 0x55 = do -- getbyte
  logicSigVersionGE 3 "getbyte"
  b <- pop
  a <- pop
  getbyte a b >>= push
  
{-
execute 0x56 = stub "setbyte"
execute 0x57 = stub "extract"
execute 0x58 = stub "extract3"
execute 0x59 = stub "extract16bits"
execute 0x5a = stub "extract32bits"
execute 0x5b = stub "extract64bits"
-}
-- 0x5c, 0x5d, 0x5e, 0x5f
execute 0x60 = do
  logicSigVersionGE 2 "balance"
  inMode Application "balance"
  (pop >>= balance) >>= push
{-
execute 0x61 = stub "app_opted_in"
-}
execute 0x62 = do -- app_local_get
  logicSigVersionGE 2 "app_local_get"
  inMode Application "app_local_get"
  b <- pop
  a <- pop
  appLocalGet a b >>= push
  
{-
execute 0x63 = stub "app_local_get_ex"
-}
execute 0x64 = do
  logicSigVersionGE 2 "app_global_get"
  inMode Application "app_global_get"
  (pop >>= appGlobalGet) >>= push
execute 0x65 = do
  logicSigVersionGE 2 "app_global_get_ex"
  inMode Application "app_global_get_ex"
  key <- pop
  offset_id <- pop
  vhuh <- appGlobalGetEx offset_id key
  case vhuh of
    Just value -> do
      push value
      push 1
    Nothing -> do
      push 0
      push 0
execute 0x66 = do
  logicSigVersionGE 2 "app_local_put"
  inMode Application "app_local_put"
  c <- pop
  b <- pop
  a <- pop
  appLocalPut a b c

execute 0x67 = do
  logicSigVersionGE 2 "app_global_put"
  inMode Application "app_global_put"
  b <- pop
  a <- pop
  appGlobalPut a b
execute 0x68 = do
  logicSigVersionGE 2 "app_local_del"
  inMode Application "app_local_del"
  b <- pop
  a <- pop
  appLocalDel a b
{-
execute 0x69 = stub "app_global_del"
-}
execute 0x70 = do
  logicSigVersionGE 2 "app_global_put"
  inMode Application "app_global_put"
  hfi <- readUint8
  asst <- pop
  acct <- pop
  vhuh <- assetHoldingGet acct asst hfi
  case vhuh of
    Just value -> do
      push value
      push 1
    Nothing -> do
      push 0
      push 0
execute 0x71 = do
  logicSigVersionGE 2 "asset_params_get"
  inMode Application "asset_params_get"
  api <- readUint8
  ano <- pop
  vhuh <- assetParamsGet ano api
  case vhuh of
    Just value -> do
      push value
      push 1
    Nothing -> do
      push 0
      push 0
execute 0x72 = stub "app_params_get"
-- 0x73, 0x74, 0x75, 0x76, 0x77
execute 0x78 = do
  logicSigVersionGE 3 "min_balance"
  inMode Application "min_balance"
  (pop >>= minBalance) >>= push
-- 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f
execute 0x80 = do -- pushbytes
  logicSigVersionGE 3 "pushbytes"
  readBytes >>= push . fromBytes
execute 0x81 = do -- pushint
  logicSigVersionGE 3 "pushint"
  readVaruint >>= push . fromInteger
-- 0x82, 0x83, 0x84, 0x85, 0x86, 0x87
{-
execute 0x88 = do -- callsub
  logicSigVersionGE 4 "callsub"
  readInt16 >>= callsub
execute 0x89 = do -- retsub
  logicSigVersionGE 4 "retsub"
  retsub
execute 0x90 = stub "shl"
execute 0x91 = stub "shr"
execute 0x92 = stub "sqrt"
execute 0x93 = stub "bitlen"
execute 0x94 = stub "exp"
execute 0x95 = stub "expw"
-- 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f
execute 0xa0 = stub "b+"
execute 0xa1 = stub "b-"
execute 0xa2 = stub "b/"
execute 0xa3 = stub "b*"
execute 0xa4 = stub "b<"
execute 0xa5 = stub "b>"
execute 0xa6 = stub "b<="
execute 0xa7 = stub "b>="
execute 0xa8 = stub "b=="
execute 0xa9 = stub "b!="
execute 0xaa = stub "b%"
execute 0xab = stub "b|"
execute 0xac = stub "b&"
execute 0xad = stub "b^"
execute 0xae = stub "b~"
execute 0xaf = stub "bzero"
-}
execute 0xb0 = do -- log
  logicSigVersionGE 5 "log"
  pop >>= log
-- the rest are unused
execute oc = unused oc

