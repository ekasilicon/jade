module ReadByte
  ( ReadByte
  , readByte
  , readOpcode
  , readUint8
  , readInt16
  , readVaruint
  , readIntcblock
  , readBytes
  , readBytecblock )
  where

import Control.Monad
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

readIntcblock :: (ReadByte m) => m [Integer]
readIntcblock = readVaruint >>= loop
  where loop n = if n == 0
                 then return []
                 else do
          x <- readVaruint
          xs <- loop $ n - 1
          return $ x:xs

readBytes :: (ReadByte m) => m [Word8]
readBytes = readVaruint >>= loop
  where loop n = if n == 0
                 then return []
                 else do
          b <- readByte
          bs <- loop $ n - 1
          return $ b:bs

readBytecblock :: (ReadByte m) => m [[Word8]]
readBytecblock = readVaruint >>= loop
  where loop n = if n == 0
                 then return []
                 else do
          bs <- readBytes
          bss <- loop $ n - 1
          return $ bs:bss
