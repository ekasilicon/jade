module VM where

import Data.Functor
import Control.Monad (liftM)
import ReadByte

type Code = Integer

data Result s a = Partial s a | Success Code | Failure String

data VM s a = ThisVM (s -> Result s a)

instance Functor (VM s) where
  fmap = liftM

instance Applicative (VM s) where
  pure x = ThisVM $ \s -> Partial s x

instance Monad (VM s) where
  (ThisVM m) >>= f = ThisVM $ \s -> case (m s) of
                                      Partial s x -> let ThisVM m = f x in m s
                                      Success c -> Success c
                                      Failure s -> Failure s

instance ReadByte (VM s) where
  readByte = ThisVM $ \s -> Partial s 42

--class Monad m => VM m s a where
--  push :: Integer -> m a

isZero x = ThisVM $ \s -> Partial s True

readBytes = ThisVM $ \s -> Partial s 42

continue = ThisVM $ \s -> Partial s 42

finish x = ThisVM $ \s -> Success x
fail msg = ThisVM $ \s -> Failure msg

jump x = ThisVM $ \s -> Partial s 42

transaction fi = ThisVM $ \s -> Partial s 42

global i = ThisVM $ \s -> Partial s 42

load i = ThisVM $ \s -> Partial s 42
store i x = ThisVM $ \s -> Partial s 42

group_load gi i = ThisVM $ \s -> Partial s 42

transaction_array fi fai = ThisVM $ \s -> Partial s 42

group_transaction gi fi = ThisVM $ \s -> Partial s 42
group_transaction_array gi fi fai = ThisVM $ \s -> Partial s 42

argument i = ThisVM $ \s -> Partial s 42

intcblock i = ThisVM $ \s -> Partial s 42
putIntcblock xs = ThisVM $ \s -> Partial s 42

bytecblock i = ThisVM $ \s -> Partial s 42
putBytecblock bss = ThisVM $ \s -> Partial s 42

data Mode = LogicSig | Application
  deriving Eq

inMode mode = ThisVM $ \s -> Partial s 42
