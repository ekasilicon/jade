
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
module VM2 where

import Data.Word (Word8)

data Mode

class (Monad m) => VM m s where
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
  le :: s -> s -> m Bool
  isZero :: s -> m Bool
  jump :: Integer -> m ()
  global :: Integer -> m s
  transaction :: Integer -> m s
  transactionArray :: Integer -> Integer -> m s
  groupTransaction :: Integer -> Integer -> m s
  groupTransactionArray :: Integer -> Integer -> Integer -> m s
  store :: Integer -> s -> m ()
  load :: Integer -> m s
  appGlobalGet :: s -> m s
  appGlobalPut :: s -> s -> m ()
  appGlobalGetEx :: s -> s -> m (Maybe s)
  appLocalGet :: s -> s -> m s
  appLocalPut :: s -> s -> s -> m ()
  assetHoldingGet :: s -> s -> Integer -> m (Maybe s)
  keccak256 :: s -> m s
  itob :: s -> m s
  btoi :: s -> m s
  substring :: Integer -> Integer -> s -> m s
  substring3 :: s -> s -> s -> m s
  concat :: s -> s -> m s
  getbyte :: s -> s -> m s


