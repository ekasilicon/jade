module Abstract ( analyze ) where

import Prelude hiding (fail)
import Data.Word (Word8)
import Data.Set (Set, empty, member, insert)
import Mode
import ReadByte
import Prefix
import Control.Applicative
import Control.Monad hiding (fail)
import VM
-- import Trace

data Value = Value
  deriving (Eq, Ord, Show)

instance Num Value where
  _ + _ = undefined
  _ * _ = undefined
  abs _ = undefined
  signum _ = undefined
  negate _ = undefined
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

type AVMa a = Bool -> State -> [AResult a]

newtype AVM a = AVM (AVMa a)

instance Alternative AVM where
  empty = AVM $ \_ _ -> []
  m0 <|> m1 = AVM $ aMplus m0 m1
    where aMplus (AVM ac0) (AVM ac1) pr st = (ac0 pr st) ++ (ac1 pr st)

instance Functor AVM where
  fmap = undefined
  
instance Applicative AVM where
  pure = undefined
  liftA2 = undefined

  
instance Monad AVM where
  return x = AVM $ \_ st -> [Partial x st]
  (AVM ac) >>= f = AVM aBind
    where aBind pr st = mconcat . map bind $ ac pr st
            where bind (Partial x st') = let AVM ac' = f x in ac' pr st'
                  bind (Success code) = [Success code]
                  bind (Failure msg)  = [Failure msg]
                   
instance MonadPlus AVM

aReadByte :: AVMa Word8
aReadByte _ st@(State { program = (Bytecode ws)
                       , pc = pc }) =
  if | pc < 0 -> [Failure "tried to read before program start"]
     | pc >= length ws -> [Failure "tried to read after program end"]
     | otherwise -> [Partial (ws !! pc) st{ pc = pc + 1 }]

instance ReadByte AVM where
  readByte = AVM aReadByte

aFail :: String -> AVMa a
aFail msg _ _ = [Failure msg]

aFinalize :: AVMa ()
aFinalize _ st = finalize $ stack st
  where finalize [_] = [Success 1, Success 0]
        finalize stk = [Failure $ "expected stack with one value; got " ++ (show stk)]

mCheckFinal :: AVM ()
mCheckFinal = do
  pc <- programCounter
  n <- programLength
  if pc == n
    then AVM aFinalize
    else continue

aPutIntcblock :: [Integer] -> AVMa ()
aPutIntcblock ib _ st = [Partial () st{ intcBlock = ib }]

aIntcblock :: Integer -> AVMa Value
aIntcblock i _ st@State{ intcBlock = ib } =
  if | (fromInteger i) < length ib -> [Partial Value st]
     | otherwise -> [Failure "integer constant access out of bounds"]

aPutBytecblock :: [[Word8]] -> AVMa ()     
aPutBytecblock bb _ st = [Partial () st{ bytecBlock = bb }]

aBytecblock :: Integer -> AVMa Value
aBytecblock i _ st@State{ bytecBlock = bb } =
  if | (fromInteger i) < length bb -> [Partial Value st]
     | otherwise -> [Failure "byte constant access out of bounds"]
     
aLogicSigVersion :: AVMa Integer
aLogicSigVersion _ st = [Partial (logicSignatureVersion st) st]

aPush :: Value -> AVMa ()
aPush x _ st@State{ stack = stk } = [Partial () st{ stack = x:stk }]

aPop :: AVMa Value
aPop _    State{ stack = [] } = [Failure "attempt to pop with empty stack"]
aPop _ st@State{ stack = x:stk } = [Partial x st{ stack = stk }]

mGlobal :: Integer -> AVM Value
mGlobal 3 = return Value -- $ Bytes $ replicate 32 0 ZeroAddress
mGlobal 4 = return Value -- $ GroupSize
mGlobal 6 = do
  logicSigVersionGE 2 "Round"
  return Value
mGlobal 9 = do
  logicSigVersionGE 3 "CreatorAddress"
  return Value
mGlobal gi = fail $ "XXX need to handle global index " ++ (show gi)

mTransaction :: Integer -> AVM Value
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

programCounter :: AVM Int
programCounter = AVM $ \_ st -> [Partial (pc st) st]

putProgramCounter :: Int -> AVM ()
putProgramCounter pc = AVM $ \_ st -> [Partial () st{ pc = pc }]

programLength :: AVM Int
programLength = AVM $ \_ st@State{ program = Bytecode ws } -> [Partial (length ws) st]

goto :: Int -> AVM ()
goto pc = if pc < 0
               then fail $ "cannot go to negative program counter (" ++ (show pc) ++ ")"
               else do
  n <- programLength
  if pc > n
    then fail $ "cannot go to program counter " ++ (show pc) ++ " which exceeds program length " ++ (show n)
    else do
    lsv <- logicSigVersion
    priv <- privileged
    if lsv < 2 && pc == n && not priv
      then fail $ "cannot go to program counter " ++ (show pc) ++ " which is program length in version 1"
      else putProgramCounter pc

mJump :: Int -> AVM ()
mJump offset = do
  lsv <- logicSigVersion
  priv <- privileged
  case lsv < 4 && offset < 0 && not priv of
    True -> fail $ "negative offset (" ++ (show offset) ++ ") requires version 4; using version " ++ (show lsv)
    False -> do
      pc <- programCounter
      goto $ pc + offset


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
  add _ _ = mplus (return Value) (fail "+ overflow")
  sub _ _ = mplus (return Value) (fail "- overflow")
  div _ _ = mplus (return Value) (fail "/ by 0")
  mul _ _ = mplus (return Value) (fail "* overflow")
  mod _ _ = mplus (return Value) (fail "% by 0")
  eq _ _ = mplus (fail "equality comparison between incompatible types") $ mplus (return True) (return False)
  lt _ _ = mplus (fail "inequality comparison between non-uint64 types") $ mplus (return True) (return False)
  isZero _ = mplus (mplus (return True) (return False)) (fail "test of whether bytes are zero")
  jump offset = mJump $ fromInteger offset
  finish _ = AVM $ \_ _ -> [Success 1, Success 0]
  global = mGlobal
  transaction = mTransaction
  transactionArray _ _ = return Value
  groupTransaction _ _ = return Value
  groupTransactionArray _ _ _ = return Value
  store _ _ = continue -- AVM $ aStore i x
  load _ = return Value
  appGlobalGet _ = return Value
  appGlobalPut _ _ = continue
  appGlobalGetEx _ _ = mplus (return $ Just Value) (return Nothing)
  appLocalGet _ _ = return Value
  appLocalPut _ _ _ = return ()
  assetHoldingGet _ _ _ = mplus (return $ Just Value) (return Nothing)
  keccak256 _ = mplus (return Value) (fail "keccak256 requires bytes")
  itob _ = return Value
  btoi _ = mplus (return Value) (fail "btoi fail: greater than eight bytes")
  substring _ _ _ = mplus (return Value) (fail "substring fail: bad indices")
  substring3 _ _ _  = mplus (return Value) (fail "substring fail: bad indices")
  concat _ _ = mplus (return Value) (fail "concat fail: overflow")
  getbyte _ _ = mplus (return Value) (fail "getbyte fail: overflow (see Go VM's behavior)")

inject :: Integer -> [Word8] -> State
inject lsv ws = State (Bytecode ws) 0 [] lsv [] []

step :: VM m s => m ()
step = do
  readOpcode >>= execute
  checkFinal

-- peek :: VM m s => m (Int, Integer)
{-
peek :: AVM (Int, Integer)
peek = do
  pc <- programCounter
  oc <- readOpcode
  return (pc,oc)
-}

run :: AVM a -> State -> [AResult a]
run (AVM ac) st = ac False st

{-
process2 :: AResult () -> IO ()
process2 (Failure s) = return () -- putStrLn $ "  FAIL: " ++ s
process2 (Success i) = return () -- putStrLn $ "  HALT: " ++ (show i)
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
doit2 (Partial (pc,oc) _) = do
  putStrLn $ show pc
  trace oc

doit [] = return ()
doit (r:rs) = do
  doit2 r
  doit rs

go2 :: State -> IO ()
go2 s = do
  -- doit $ run peek s
  process $ run step s
        

go :: Integer -> [Word8] -> IO ()
go lsv ws = go2 $ inject lsv ws
-}
{-
analyze :: [Word8] -> IO ()
analyze ws = let Read m = readPrefix in case m ws of
  Just (lsv,ws) -> go lsv ws
  Nothing -> putStrLn "some error"
-}

go3 :: [AResult ()] -> [State] -> Set State -> [String] -> (Set State, [String])
go3 [] todo seen errs = go2 todo seen errs
go3 (r:rs) todo seen errs = case r of
                              Partial () st -> go3 rs (st:todo) seen errs
                              Success _ -> go3 rs todo seen errs
                              Failure msg -> go3 rs todo seen (msg:errs)

go2 :: [State] -> Set State -> [String] -> (Set State, [String])
go2 [] seen errs = (seen,errs)
go2 (st:todo) seen errs =
  case member st seen of
    True -> go2 todo seen errs
    False -> go3 (run step st) todo (insert st seen) errs

go :: State -> (Set State, [String])
go st = go2 [st] Data.Set.empty []

analyze :: [Word8] -> IO ()
analyze ws = let Read m = readPrefix in case m ws of
  Just (lsv,ws') -> let (_,errs) = go $ inject lsv ws'
                    in emit errs
    where emit [] = return ()
          emit (e:errs) = do
            putStrLn e
            emit errs
  Nothing -> putStrLn "couldn't read logicSigVersion"


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
