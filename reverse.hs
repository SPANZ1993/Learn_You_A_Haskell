import Control.Monad
import Data.Char (toUpper)

main = do
  line <- getLine
  if null line
    then do
      putStrLn "Write three lines Burt"
      rs <- sequence [getLine, getLine, getLine]
      print rs
      return ()
    else do
      when (map toUpper line == "EGG") $ do
        putStrLn "HOORAY"
      putStrLn $ reverseWords line
      main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words