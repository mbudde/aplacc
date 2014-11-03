module Main where

import System.Environment
import APLAcc.TAIL.Parser (parseFile)
import APLAcc.Conversion (convertProgram)
import APLAcc.SimpleAcc.ToHaskell (toHs, OutputOpts(..), defaultOpts)

main :: IO ()
main =
  do args <- getArgs
     case parseArgs args defaultOpts of
       ([f], opts) -> parseFile f >>= putStrLn . toHs opts . convertProgram
       _           -> putStrLn "usage: aplacc [-c|--cuda] <file>"
  where parseArgs ("-c" : rest) opts     = parseArgs rest (opts { toCUDA = True })
        parseArgs ("--cuda" : rest) opts = parseArgs rest (opts { toCUDA = True })
        parseArgs (x : rest) opts        = let (as, opts') = parseArgs rest opts
                                           in (x:as, opts')
        parseArgs [] opts = ([], opts)

