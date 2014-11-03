module Main where

import System.Environment
import APLAcc.TAIL.Parser (parseFile)
import APLAcc.Conversion (convertProgram)
import APLAcc.SimpleAcc.ToHaskell (toHs)

main :: IO ()
main =
  do args <- getArgs
     case args of
       [f] -> parseFile f >>= putStrLn . toHs . convertProgram
       _   -> putStrLn "usage: aplacc <file>"

