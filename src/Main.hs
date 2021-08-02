import System.Environment (getArgs)
import Data.ByteString as DB (ByteString, readFile, unpack)
import Abstract

main :: IO ()
main = getArgs >>= process
  where
    process [] = print "done"
    process (arg:args) = do
      print arg
      bs <- DB.readFile arg
      print $ show $ analyze $ unpack bs
      process args
