module Main where

import Data.Maybe (fromMaybe, maybeToList)
import Data.List (intercalate)
import Control.Monad (when)
import System.IO (IOMode(ReadMode), withFile, stdin, Handle, hGetContents)
import System.Environment
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import APLAcc.TAIL.Parser (parseString)
import APLAcc.Conversion (convertProgram)
import APLAcc.SimpleAcc.ToHaskell (toHs, OutputOpts(..), defaultOpts)


main :: IO ()
main =
  do args <- getArgs
     case parseArgs args defaultOpts of
       (["-"], opts) -> compileFile "stdin" opts stdin
       ([f],   opts) -> withFile f ReadMode (compileFile f opts)
       _             -> putStrLn "usage: aplacc [-c|--cuda] <file>"
  where parseArgs ("-c"        : rest) opts = parseArgs rest (opts { toCUDA = True })
        parseArgs ("--cuda"    : rest) opts = parseArgs rest (opts { toCUDA = True })
        parseArgs ("-t"        : rest) opts = parseArgs rest (opts { tailInput = True })
        parseArgs ("--tail"    : rest) opts = parseArgs rest (opts { tailInput = True })
        parseArgs ("--run"     : rest) opts = parseArgs rest (opts { runProgram = True })
        parseArgs ("-v"        : rest) opts = parseArgs rest (opts { verbose = True })
        parseArgs ("--verbose" : rest) opts = parseArgs rest (opts { verbose = True })
        parseArgs ("--"        : rest) opts = (rest, opts)
        parseArgs (x : rest) opts = let (as, opts') = parseArgs rest opts
                                    in  (x:as, opts')
        parseArgs [] opts = ([], opts)

        compileFile file opts h =
          do tailText <- if tailInput opts
                           then hGetContents h
                           else compileApl opts h
             tailProg <- parseString tailText file
             let hsText = toHs opts $ convertProgram tailProg
             when (verbose opts) $ putStrLn "\ESC[33m[ Accelerate output ]\ESC[0m"
             putStrLn hsText
             when (runProgram opts) $ runGhc opts hsText

compileApl :: OutputOpts -> Handle -> IO String
compileApl opts handle =
  do input <- hGetContents handle
     prelude <- lookupEnv "APLT_PRELUDE" >>= return . maybeToList
     aplt <- lookupEnv "APLT" >>= return . fromMaybe "aplt"
     let args = apltArgs ++ prelude ++ ["-"]
     when (verbose opts) $
       putStrLn $ "\ESC[33m[ Running \"" ++ intercalate " " (aplt : args) ++ "\" ]\ESC[0m"
     (exitcode, stdout, _) <- readProcessWithExitCode aplt args input
     when (verbose opts) $
       putStrLn "\ESC[33m[ APLT output ]\ESC[0m" >>
       putStrLn stdout
     case exitcode of
       ExitSuccess -> return stdout
       ExitFailure n -> error $ "aplt failed with code " ++ show n
  where apltArgs = ["-c", "-s_tail", "-p_tail", "-p_types", "-silent"]

runGhc :: OutputOpts -> String -> IO ()
runGhc opts program =
  do when (verbose opts) $ putStrLn "\ESC[33m[ Running GHC ]\ESC[0m"
     args <- getArgs
     when (verbose opts) $
       putStrLn $ "\ESC[33m[ Running \"" ++ intercalate " " ("runghc" : args) ++ "\" ]\ESC[0m"
     (exitcode, stdout, stderr) <- readProcessWithExitCode "runghc" args program
     case exitcode of
       ExitSuccess -> putStr stdout
       ExitFailure n ->
         do putStr stderr
            error $ "ghc failed with code " ++ show n
  where getArgs =
          do home <- lookupEnv "APLACC_HOME" >>=
                       return . fromMaybe (error "APLACC_HOME env var not set")
             return ["-i"++home++"/src"]
