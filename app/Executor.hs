module Executor ( main ) where
import System.Environment (getArgs)
import Data.ByteString as DB (readFile, unpack)
import Concrete

main :: IO ()
main = getArgs >>= process
  where
    process [] = return ()
    process (arg:args) = do
      putStrLn arg
      bs <- DB.readFile arg
      _ <- executeTransactionGroup $ unpack bs
      process args
