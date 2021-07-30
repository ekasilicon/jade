{-# LANGUAGE FlexibleInstances #-}
import Prelude hiding (Read)
import System.Environment (getArgs)
import Data.ByteString as DB (ByteString, readFile, unpack)
import Data.Word (Word8)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import ReadByte
import Exec

--type Prop = Int

--data Result s a = Partial s a | Failure String | Success Prop

-- instance Monad (Result s) where
--   return x s = Partial s x

-- type MyState = ([Word8],Int)

-- data MyMonad a = Yep (StateT Int Identity Int)

--instance ReadByte (StateT Int Identity) where
--  readByte = return 10

-- → :: State -> Result s a

-- → :: 

-- → s = let ((),runState s

-- analyze :: TEAL -> LatticeValue
--analyze bs =
--  let (tlv,pgm) = extract bs in
--    let 

type ReadResult a = Maybe (a,[Word8])
newtype Read a = Read ([Word8] -> ReadResult a)

instance Functor Read where
  fmap f (Read ac) = Read g
    where g ws = case ac ws of
            Just (x,ws') -> Just (f x,ws')
            Nothing -> Nothing

instance Applicative Read where
  pure x = Read $ \ws -> Just (x,ws)
  (Read af) <*> (Read ax) = Read $ \ws -> case af ws of
    Just (f,ws') -> case ax ws' of
      Just (x,ws'') -> Just(f x,ws'')
      Nothing -> Nothing
    Nothing -> Nothing

instance Monad Read where
  (Read m) >>= f = Read g
    where g ws = case m ws of
            Just (x,ws') -> let Read m' = (f x) in m' ws'
            Nothing -> Nothing

instance ReadByte Read where
  readByte = Read g
    where g (w:ws) = Just (w,ws)
          g [] = Nothing

readPrefix :: ReadByte m => m Integer
readPrefix = readVaruint

analyze :: [Word8] -> Integer
analyze ws = let Read m = readPrefix in case m ws of
  Just (x,ws) -> x
  Nothing -> 42

main :: IO ()
main = getArgs >>= process
  where
    process [] = print "done"
    process (arg:args) = do
      print arg
      bs <- DB.readFile arg
      print $ show $ analyze $ unpack bs
      process args
