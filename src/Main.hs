module Main where

import Data.Maybe (fromMaybe, maybeToList)
import Data.List (intercalate)
import Control.Monad (when)
import System.IO (IOMode(ReadMode), withFile, stdin, stderr, Handle, hPutStr, hPutStrLn, hGetContents)
import System.Environment
import System.Process (readProcessWithExitCode)
import System.Exit (exitFailure, ExitCode(..))

import APLAcc.TAIL.Parser (parseString)
import APLAcc.Conversion (convertProgram)
import APLAcc.SimpleAcc.ToHaskell (toHs, OutputOpts(..), defaultOpts)

putErr = hPutStr stderr
putErrLn = hPutStrLn stderr

usage :: IO ()
usage =
  do putErrLn "Usage: aplacc [options] <file>\n\n\
              \  -v, --verbose          verbose output\n\
              \  -t, --tail             input file is a TAIL program\n\
              \  -c, --cuda             use CUDA backend\n\
              \  --run                  run Accelerate program"
     exitFailure

main :: IO ()
main =
  do args <- getArgs
     case parseArgs args defaultOpts of
       (["-"], opts) -> compileFile "stdin" opts stdin
       ([f],   opts) -> withFile f ReadMode (compileFile f opts)
       _             -> usage
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
             when (verbose opts) $ putErrLn "\ESC[33m[ Accelerate output ]\ESC[0m"
             when (verbose opts || not (runProgram opts)) $ putStrLn hsText
             when (runProgram opts) $ runGhc opts hsText

compileApl :: OutputOpts -> Handle -> IO String
compileApl opts handle =
  do input <- hGetContents handle
     prelude <- lookupEnv "APLT_PRELUDE" >>= return . maybeToList
     aplt <- lookupEnv "APLT" >>= return . fromMaybe "aplt"
     let args = apltArgs ++ prelude ++ ["-"]
     when (verbose opts) $
       putErrLn $ "\ESC[33m[ Running aplt ] " ++ intercalate " " (aplt : args) ++ "\ESC[0m"
     (exitcode, stdout, _) <- readProcessWithExitCode aplt args input
     when (verbose opts) $
       putErrLn "\ESC[33m[ APLT output ]\ESC[0m" >>
       putErrLn stdout
     case exitcode of
       ExitSuccess -> return stdout
       ExitFailure n ->
         do putErr stdout
            putErrLn $ "aplacc: aplt failed with code " ++ show n
            exitFailure
  where apltArgs = ["-c", "-s_tail", "-p_tail", "-p_types", "-silent"]

runGhc :: OutputOpts -> String -> IO ()
runGhc opts program =
  do args <- getArgs
     when (verbose opts) $
       putErrLn $ "\ESC[33m[ Running with GHC ] " ++ intercalate " " ("runghc" : args) ++ "\ESC[0m"
     (exitcode, stdout, stderr) <- readProcessWithExitCode "runghc" args program
     case exitcode of
       ExitSuccess -> putErr stdout
       ExitFailure n ->
         do putErr stderr
            putErrLn $ "aplacc: ghc failed with code " ++ show n
            exitFailure
  where getArgs =
          do home <- lookupEnv "APLACC_HOME"
             case home of
               Just dir -> return ["-i"++dir++"/src"]
               Nothing  -> return []
