module Prefix where

import Prelude hiding (Read)
import Data.Word (Word8)
import Control.Monad
import ReadByte

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
