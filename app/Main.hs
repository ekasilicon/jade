import System.Environment (getArgs)
import Data.ByteString as DB (readFile, unpack)
import Abstract

main :: IO ()
main = getArgs >>= process
  where
    process [] = return ()
    process (arg:args) = do
      putStrLn arg
      bs <- DB.readFile arg
      analyze $ unpack bs
      {-
      case analyze $ unpack bs of
        Just (_, rs) -> go rs
          where go [] = return ()
                go (x:xs) = putStrLn ("  " ++ x) >> go xs
        Nothing -> print "failed"
      -}
      process args
