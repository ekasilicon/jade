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

{-
data Symbol = RekeyTo
  deriving (Eq, Ord, Show)

data Value = Uint64 Integer | Bytes [Word8] | Symbolic Symbol
  deriving (Eq, Ord, Show)

-}

data Value = Value
  deriving (Eq, Ord, Show)

instance Num Value where
  fromInteger _ = Value

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

newtype AVM a = AVM (State -> [AResult a])

aFmap f ac st = map bid $ ac st
  where bid (Partial x st') = Partial (f x) st'
        bid (Success code)  = Success code
        bid (Failure msg)   = Failure msg

instance Functor AVM where
  fmap f (AVM ac) = AVM $ aFmap f ac

aPure x st = [Partial x st]

instance Applicative AVM where
  pure x = AVM $ aPure x

aBind ac f st = mconcat . map bid $ ac st
  where bid (Partial x st') = let AVM ac' = f x in ac' st'
        bid (Success code)  = [Success code]
        bid (Failure msg)   = [Failure msg]

instance Monad AVM where
  (AVM ac) >>= f = AVM $ aBind ac f

aMzero _ = []
aMplus (AVM ac0) (AVM ac1) s = (ac0 s) ++ (ac1 s)

instance Alternative AVM where
  empty = AVM aMzero
  m0 <|> m1 = AVM $ aMplus m0 m1

instance MonadPlus AVM

aReadByte s@(State { program = (Bytecode ws)
                   , pc = pc }) =
  if | pc < 0 -> [Failure "tried to read before program start"]
     | pc >= length ws -> [Failure "tried to read after program end"]
     | otherwise -> [Partial (ws !! pc) s{ pc = pc + 1 }]

instance ReadByte AVM where
  readByte = AVM aReadByte

aFail msg _ = [Failure msg]
aFinalize2 [x] = [Success 1, Success 0]
aFinalize2 stk = [Failure "expected stack with one value; got "]
aFinalize s = aFinalize2 $ stack s
mCheckFinal = do
  pc <- programCounter
  n <- programLength
  if pc == n
    then AVM aFinalize
    else continue
aPutIntcblock ib s = [Partial () s{ intcBlock = ib }]
aIntcblock i s@State{ intcBlock = ib } =
  if | (fromInteger i) < length ib -> [Partial Value s]
     | otherwise -> [Failure "integer constant access out of bounds"]
aPutBytecblock bb s = [Partial () s{ bytecBlock = bb }]
aBytecblock i s@State{ bytecBlock = bb } =
  if | (fromInteger i) < length bb -> [Partial Value s]
     | otherwise -> [Failure "byte constant access out of bounds"]
aLogicSigVersion s = [Partial (logicSignatureVersion s) s]
aPush x s@State{ stack = stk } = [Partial () s{ stack = x:stk }]
aPop s@State{ stack = [] } = [Failure "attempt to pop with empty stack"]
aPop s@State{ stack = x:stk } = [Partial x s{ stack = stk }]

mEq x y = mplus (return Value) (fail "equality comparison between incompatible types")

mLe x y = mplus (return Value) (fail "inequality comparison between non-uint64 types")

mIsZero x = mplus (mplus (return True) (return False)) (fail "test of whether bytes are zero")

mGlobal 3 = return Value -- $ Bytes $ replicate 32 0 ZeroAddress
mGlobal 4 = return Value -- $ GroupSize
mGlobal 6 = do
  logicSigVersionGE 2 "Round"
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
mTransaction 29 = do
  logicSigVersionGE 2 "NumAccounts"
  return Value
mTransaction 32 = do
  logicSigVersionGE 2 "RekeyTo"
  return $ Value -- Symbolic RekeyTo
mTransaction fi = fail $ "XXX need to handle transaction field index " ++ (show fi)

mTransactionArray _ _ = return Value

{-
datumCase x onu onb = case x of
  Uint64 x -> onu x
  Bytes x -> onb x
  Symbolic x -> case x of
                  RekeyTo -> onb RekeyTo

mEq a b = datumCase a
          (\ua ->
              datumCase b
              (\ub -> return $ Uint64 $ if ua == ub then 1 else 0)
              (\bb -> fail "A is uint64 but B is bytes"))
          (\ba ->
              datumCase b
              (\ub -> fail "A is bytes but B is uint64")
              (\bb -> return $ Uint64 $ if ba == bb then 1 else 0))



mIsZero x = datumCase x
  (\u -> return $ u == 0)
  (\b -> fail "A is bytes but should be uint64")
-}

aProgramCounter s = [Partial (pc s) s]
programCounter = AVM aProgramCounter

aPutProgramCounter pc s = [Partial () s{ pc = pc }]
putProgramCounter pc = AVM $ aPutProgramCounter pc

aProgramLength s@State{ program = Bytecode ws } = [Partial (length ws) s]
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

aFinish x s = [Success 1, Success 0]

-- aStore i x (State ws pc stk ss lsv ib bb) = 

instance VM AVM Value where
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
  appLocalGet app key = return Value
  appLocalPut app key value = return ()
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
    
run :: AVM () -> State -> [AResult ()]
run (AVM ac) st = ac st
  
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
