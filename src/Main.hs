module Main where

import System.Environment
import Tail.Parser
import Tail.Converter

main :: IO ()
main =
  do args <- getArgs
     case args of
       [f] -> parseFile f >>= putStrLn . prettyPrintProgram
       _   -> putStrLn "usage: aplacc <file>"

