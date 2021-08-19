import System.Directory (listDirectory)
import Data.ByteString as DB (readFile, unpack)
import Abstract

main :: IO ()
main = listDirectory "test/algoexplorer/extracted" >>= process
  where
    process [] = return ()
    process (path:paths) = do
      putStrLn path
      bs <- DB.readFile $ "test/algoexplorer/extracted/" ++ path
      analyze $ unpack bs
      {-
      case analyze $ unpack bs of
        Just (_, rs) -> go rs
          where go [] = return ()
                go (x:xs) = putStrLn ("  " ++ x) >> go xs
        Nothing -> print "failed"
      -}
      process paths
