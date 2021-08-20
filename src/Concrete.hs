module Concrete ( executeTransactionGroup )
where

import Data.Word (Word8)
-- import ReadByte
import Prefix
-- import VM

data Value = Uint64 Integer | Bytes [Word8]



-- instance VM 

executeTransactionGroup :: a
executeTransactionGroup = undefined

inject :: p1 -> p2 -> a
inject _ _ = undefined

analyze :: [Word8] -> IO ()
analyze ws = let Read m = readPrefix in case m ws of
  Just (lsv,ws') -> executeTransactionGroup $ inject lsv ws'
  Nothing -> putStrLn "couldn't read logicSigVersion"

