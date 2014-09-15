
import System.Environment
import Tail.Parser

main :: IO ()
main =
  do args <- getArgs
     case args of
       [f] -> parseFile f
       _   -> putStrLn "usage: aplacc <file>"

