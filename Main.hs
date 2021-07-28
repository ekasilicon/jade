import System.Environment (getArgs)
import Data.ByteString as DB (ByteString, readFile, unpack)
import Data.Word (Word8)
import Control.Monad
import Control.Monad.State

newtype MyState = ([Word8],Int)

readVaruint :: State MyState Integer
readVaruint (bs,i) = 
  
  


analyze :: [Word8] -> Integer
analyze _ = let eval st = 5
  in eval 1
  

main :: IO ()
main = getArgs >>= process
  where
    process [] = print "done"
    process (arg:args) = do
      print arg
      bs <- DB.readFile arg
      print $ show $ analyze $ unpack bs
      process args
