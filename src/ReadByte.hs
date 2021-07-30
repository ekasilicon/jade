module ReadByte where

import Control.Monad (Monad,liftM)
import Data.Word
import Data.Bits

class (Monad m) => ReadByte m where
  readByte :: m Word8

readOpcode :: (ReadByte m) => m Integer
readOpcode = readUint8

readUint8 :: (ReadByte m) => m Integer
readUint8 = liftM (toInteger . fromEnum) readByte

-- big-endian
readInt16 :: (ReadByte m) => m Integer
readInt16 = do
  hi <- readUint8
  lo <- readUint8
  return $ (shiftL (if hi < 128 then hi else hi - 256) 8) .|. lo

readVaruint :: (ReadByte m) => m Integer
readVaruint = do
  b <- readUint8
  if (b .&. 0x80) == 0
    then return b
    else do
      n <- readVaruint
      return $ (shiftL n 7) .|. (b .&. 0x7F)
