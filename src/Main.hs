module Main where

import System.Environment
import Tail.Parser
import Tail.Converter2
import Tail.Output

main :: IO ()
main =
  do args <- getArgs
     case args of
       [f] -> parseFile f >>= putStrLn . prettyPrintProgram . convertProgram
       _   -> putStrLn "usage: aplacc <file>"

