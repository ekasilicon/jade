{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, MultiWayIf #-}
module Abstract where

import Prelude hiding (fail)
import Data.Word (Word8)
import Data.Set (Set, empty, member, insert)
import Mode
import ReadByte
import Prefix
import Control.Applicative
import Control.Monad hiding (fail)
import VM
import Trace

data Value = Value
  deriving (Eq, Ord, Show)

instance Num Value where
  fromInteger _ = Value

instance Bytes Value where
  fromBytes _ = Value

newtype Bytecode = Bytecode [Word8]
  deriving (Eq, Ord)

instance Show Bytecode where
  show _ = "<code>"

data State = State { program :: Bytecode
                   , pc :: Int
                   , stack :: [Value]
                   , logicSignatureVersion :: Integer
                   , intcBlock :: [Integer]
                   , bytecBlock :: [[Word8]]
                   }
  deriving (Show, Eq, Ord)

data AResult a = Partial a State | Success Integer | Failure String

newtype AVM a = AVM (Bool -> State -> [AResult a])

aFmap f ac pr st = map bid $ ac pr st
  where bid (Partial x st') = Partial (f x) st'
        bid (Success code)  = Success code
        bid (Failure msg)   = Failure msg

instance Functor AVM where
  fmap f (AVM ac) = AVM $ aFmap f ac

aPure x pr st = [Partial x st]

instance Applicative AVM where
  pure x = AVM $ aPure x

aBind ac f pr st = mconcat . map bid $ ac pr st
  where bid (Partial x st') = let AVM ac' = f x in ac' pr st'
        bid (Success code)  = [Success code]
        bid (Failure msg)   = [Failure msg]

instance Monad AVM where
  (AVM ac) >>= f = AVM $ aBind ac f

aMzero _ _ = []
aMplus (AVM ac0) (AVM ac1) pr st = (ac0 pr st) ++ (ac1 pr st)

instance Alternative AVM where
  empty = AVM aMzero
  m0 <|> m1 = AVM $ aMplus m0 m1

instance MonadPlus AVM

aReadByte pr st@(State { program = (Bytecode ws)
                       , pc = pc }) =
  if | pc < 0 -> [Failure "tried to read before program start"]
     | pc >= length ws -> [Failure "tried to read after program end"]
     | otherwise -> [Partial (ws !! pc) st{ pc = pc + 1 }]

instance ReadByte AVM where
  readByte = AVM aReadByte

aFail msg _ _ = [Failure msg]
aFinalize2 [x] = [Success 1, Success 0]
aFinalize2 stk = [Failure "expected stack with one value; got "]
aFinalize pr st = aFinalize2 $ stack st
mCheckFinal = do
  pc <- programCounter
  n <- programLength
  if pc == n
    then AVM aFinalize
    else continue
aPutIntcblock ib pr st = [Partial () st{ intcBlock = ib }]
aIntcblock i pr st@State{ intcBlock = ib } =
  if | (fromInteger i) < length ib -> [Partial Value st]
     | otherwise -> [Failure "integer constant access out of bounds"]
aPutBytecblock bb pr st = [Partial () st{ bytecBlock = bb }]
aBytecblock i pr st@State{ bytecBlock = bb } =
  if | (fromInteger i) < length bb -> [Partial Value st]
     | otherwise -> [Failure "byte constant access out of bounds"]
aLogicSigVersion pr st = [Partial (logicSignatureVersion st) st]
aPush x pr st@State{ stack = stk } = [Partial () st{ stack = x:stk }]
aPop pr st@State{ stack = [] } = [Failure "attempt to pop with empty stack"]
aPop pr st@State{ stack = x:stk } = [Partial x st{ stack = stk }]

mEq x y = mplus (return Value) (fail "equality comparison between incompatible types")

mLe x y = mplus (return Value) (fail "inequality comparison between non-uint64 types")

mIsZero x = mplus (mplus (return True) (return False)) (fail "test of whether bytes are zero")

mGlobal 3 = return Value -- $ Bytes $ replicate 32 0 ZeroAddress
mGlobal 4 = return Value -- $ GroupSize
mGlobal 6 = do
  logicSigVersionGE 2 "Round"
  return Value
mGlobal 9 = do
  logicSigVersionGE 3 "CreatorAddress"
  return Value
mGlobal gi = fail $ "XXX need to handle global index " ++ (show gi)

mTransaction  0 = do -- Sender
  return Value
mTransaction  7 = do -- Receiver
  return Value
mTransaction  8 = do -- Amount
  return Value
mTransaction  9 = do -- CloseRemainderTo
  return Value
mTransaction 16 = do -- TypeEnum
  return Value
mTransaction 22 = do -- GroupIndex
  return Value 
mTransaction 24 = do
  logicSigVersionGE 2 "ApplicationID"
  return Value
mTransaction 25 = do
  logicSigVersionGE 2 "OnCompletion"
  return Value
mTransaction 27 = do
  logicSigVersionGE 2 "NumAppArgs"
  return Value
mTransaction 29 = do
  logicSigVersionGE 2 "NumAccounts"
  return Value
mTransaction 32 = do
  logicSigVersionGE 2 "RekeyTo"
  return $ Value -- Symbolic RekeyTo
mTransaction fi = fail $ "XXX need to handle transaction field index " ++ (show fi)

mTransactionArray _ _ = return Value

aProgramCounter pr st = [Partial (pc st) st]
programCounter = AVM aProgramCounter

aPutProgramCounter pc pr st = [Partial () st{ pc = pc }]
putProgramCounter pc = AVM $ aPutProgramCounter pc

aProgramLength pr st@State{ program = Bytecode ws } = [Partial (length ws) st]
programLength = AVM aProgramLength

goto pc = if pc < 0
               then fail $ "cannot go to negative program counter (" ++ (show pc) ++ ")"
               else do
  n <- programLength
  if pc > n
    then fail $ "cannot go to program counter " ++ (show pc) ++ " which exceeds program length " ++ (show n)
    else do
    lsv <- logicSigVersion
    if lsv < 2 && pc == n
      then fail $ "cannot go to program counter " ++ (show pc) ++ " which is program length in version 1"
      else putProgramCounter pc

mJump2 offset = do
  lsv <- logicSigVersion
  if lsv < 4 && offset < 0
    then fail $ "negative offset (" ++ (show offset) ++ ") requires version 4; using version " ++ (show lsv)
    else do
      pc <- programCounter
      goto $ pc + offset
mJump offset = mJump2 $ fromInteger offset              

aFinish x pr st = [Success 1, Success 0]

instance VM AVM Value where
  runPrivileged (AVM ac) = AVM $ \_ -> \st -> ac True st
  privileged = AVM $ \pr -> \st -> [Partial pr st]
  fail msg = AVM $ aFail msg
  checkFinal = mCheckFinal
  putIntcblock ib = AVM $ aPutIntcblock ib
  intcblock i = AVM $ aIntcblock i
  putBytecblock bb = AVM $ aPutBytecblock bb
  bytecblock i = AVM $ aBytecblock i
  logicSigVersion = AVM aLogicSigVersion
  mode = mplus (return LogicSig) (return Application)
  push x = AVM $ aPush x
  pop = AVM aPop
  add x y = mplus (return Value) (fail "+ overflow")
  sub x y = mplus (return Value) (fail "- overflow")
  div x y = mplus (return Value) (fail "/ by 0")
  mul x y = mplus (return Value) (fail "* overflow")
  mod x y = mplus (return Value) (fail "% by 0")
  eq = mEq
  le = mLe
  isZero = mIsZero
  jump = mJump
  finish x = AVM $ aFinish x
  global = mGlobal
  transaction = mTransaction
  transactionArray = mTransactionArray
  groupTransaction gi fi = return Value
  groupTransactionArray gi fi fai = return Value
  store i x = continue -- AVM $ aStore i x
  load i = return Value
  appGlobalGet key = return Value
  appGlobalPut key value = continue
  appGlobalGetEx offset_id key = mplus (return $ Just Value) (return Nothing)
  appLocalGet app key = return Value
  appLocalPut app key value = return ()
  assetHoldingGet _ _ _ = mplus (return $ Just Value) (return Nothing)
  keccak256 x = mplus (return Value) (fail "keccak256 requires bytes")
  itob x = return Value
  btoi x = mplus (return Value) (fail "btoi fail: greater than eight bytes")
  substring s e bs = mplus (return Value) (fail "substring fail: bad indices")
  substring3 s e bs = mplus (return Value) (fail "substring fail: bad indices")
  concat a b = mplus (return Value) (fail "concat fail: overflow")
  getbyte a b = mplus (return Value) (fail "getbyte fail: overflow (see Go VM's behavior)")
  
inject lsv ws = State (Bytecode ws) 0 [] lsv [] []

step :: VM m s => m ()
step = do
  readOpcode >>= execute
  checkFinal

peek :: VM m s => m Integer
peek = readOpcode
    
run :: AVM a -> State -> [AResult a]
run (AVM ac) st = ac False st

process2 :: AResult () -> IO ()
process2 (Failure s) = putStrLn $ "  FAIL: " ++ s
process2 (Success i) = putStrLn $ "  HALT: " ++ (show i)
process2 (Partial () s) = go2 s

process :: [AResult ()] -> IO ()
process [] = return ()
process (r:rs) = do
  process2 r
  process rs

instance Trace IO where
  traceInstr s = putStrLn s

doit2 (Failure _) = return ()
doit2 (Success _) = return ()
doit2 (Partial oc _) = trace oc

doit [] = return ()
doit (r:rs) = do
  doit2 r
  doit rs

go2 :: State -> IO ()
go2 s = do
  doit $ run peek s
  process $ run step s
        

go :: Integer -> [Word8] -> IO ()
go lsv ws = go2 $ inject lsv ws

analyze :: [Word8] -> IO ()
analyze ws = let Read m = readPrefix in case m ws of
  Just (lsv,ws) -> go lsv ws
  Nothing -> putStrLn "some error"

{-
analyze :: [Word8] -> Maybe ((Set State),[String])
analyze ws = let Read m = readPrefix in case m ws of
  Just (lsv,ws) -> Just $ go $ inject lsv ws
    where go st0 = go2 [st0] Data.Set.empty []
          go2 [] seen errs = (seen,errs)
          go2 (st:todo) seen errs =
            if member st seen
            then go2 todo seen errs
            else go3 (run step st) todo (insert st seen) errs
            where go3 [] todo seen errs = go2 todo seen errs
                  go3 (r:rs) todo seen errs = case r of
                                                Partial () st -> go3 rs (st:todo) seen errs
                                                Success code -> go3 rs todo seen errs
                                                Failure msg -> go3 rs todo seen (msg:errs)
  Nothing -> Nothing
-}

{-

the unconstrained parameter analysis assumes that the program is known.
the program counter starts at 0 (i.e. is known initially),
the extent of each instruction is known statically (duh), and
branch offsets are static (i.e. not computed).
thus, by induction, the program counter is always concrete.

it does not assume that the program is run in a particular mode.
because so many instructions are specific to a particular mode,
an abstract interpretation will typically have an early refinement of the mode (yielding a failed and progressing branch).
it seems possible to write a program that inspects the mode and behaves accordingly, allowing a program to include instructions which require different modes.
the abstract interpreter can handle this case, even though I expect it to be extremely rare.

-}

{-

data Mode = LogicSig | Application
  deriving Eq

getMode :: 
getMode = do
  get mode as an item from a transaction
  branch using mplus into the different modes, refining each one

inMode mode = do
  actualMode <- getMode -- refines here?
  if mode == actualMode
    then continue
    else fail "requires mode " ++ (show mode) ++ "but in mode " ++ (show actualMode)

-}    
