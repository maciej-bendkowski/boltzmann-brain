{-|
 Module      : Main
 Description : Boltzmann brain executable.
 Copyright   : (c) Maciej Bendkowski, 2018

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Main
    ( main
    ) where

import Prelude hiding (fail)

import System.IO
import System.Exit
import System.Console.GetOpt
import System.Environment

import GHC.IO.Handle
import Control.Monad (when)

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

import Text.Megaparsec hiding (parse)
import qualified Data.Set as S

import Data.Boltzmann.System
import Data.Boltzmann.System.Parser
import Data.Boltzmann.System.Errors
import Data.Boltzmann.System.Warnings
import Data.Boltzmann.System.Sampler
import Data.Boltzmann.Internal.Parser
import Data.Boltzmann.Internal.Annotations

import qualified Data.Boltzmann.System.Tuner as T

import Data.Boltzmann.Compiler
import qualified Data.Boltzmann.Compiler.Haskell.Algebraic as A
import qualified Data.Boltzmann.Compiler.Haskell.Rational as R

data Flag = InputFile  String  -- ^ input file location
          | OutputFile String  -- ^ output file location
          | TuningFile String  -- ^ paganini tuning data file location
          | Force              -- ^ whether to skip some sanity check
          | Werror             -- ^ whether to treat warnings as errors
          | Help               -- ^ whether to print usage help text
            deriving (Show,Eq)

options :: [OptDescr Flag]
options = [Option "i" ["input"] (ReqArg InputFile "FILE")
            "Optional input file. If not given, stdin is used instead.",

           Option "o" ["output"] (ReqArg OutputFile "FILE")
            "Optional output file. If not given, stdout is used instead.",

           Option "t" ["tuning-data"] (ReqArg TuningFile "FILE")
            "Optional paganini tuning data file corresponding to the input specification.",

           Option "w" ["werror"] (NoArg Werror)
            "Whether to treat warnings as errors.",

           Option "f" ["force"] (NoArg Force)
            "Whether skip some sanity checks such as the well-foundedness check.",

           Option "h?" ["help"] (NoArg Help)
            "Prints this help message."]

quote :: String -> String
quote msg = "'" ++ msg ++ "'"

version :: String
version = "v1.3.1.3"

signature :: String
signature = "Boltzmann Brain " ++ version

versionHeader :: String
versionHeader = signature ++ " (c) Maciej Bendkowski and Sergey Dovgal 2018"

usageHeader :: String
usageHeader =
    unlines [ versionHeader
            , "Usage: bb {compile|sample|tune|spec} [OPTIONS...]"
            ]

compilerTimestamp :: String
compilerTimestamp = signature

inputF :: [Flag] -> Maybe String
inputF (InputFile f : _) = Just f
inputF (_:fs)            = inputF fs
inputF []                = Nothing

outputF :: [Flag] -> Maybe String
outputF (OutputFile f : _) = Just f
outputF (_:fs)             = outputF fs
outputF []                 = Nothing

tuningF :: [Flag] -> Maybe String
tuningF (TuningFile f : _) = Just f
tuningF (_:fs)             = tuningF fs
tuningF []                 = Nothing

-- | Prints a message to stderr and exits.
fail :: String -> IO a
fail msg = do
    hPutStr stderr msg
    exitWith (ExitFailure 1)

-- | Prints a message to stderr along with
--   the usage header message and exits.
fail' :: String -> IO a
fail' msg = fail $ unlines [msg] ++ usageInfo usageHeader options

-- | Prints the usage text and exists.
usage :: IO a
usage = do
    putStr $ usageInfo usageHeader options
    exitSuccess

-- | Parses the cli arguments into the command string
--   and some additional (optional) flags.
parse :: [String] -> IO (String, [Flag])
parse argv =
    case getOpt Permute options argv of
        (opts, cmds, [])
            | null cmds        -> usage
            | length cmds /= 1 -> fail' "[Error] Expected a single command."
            | Help `elem` opts -> usage
            | otherwise        -> return (head cmds, opts)

        (_, _, errs) -> fail' $ concat errs

-- | Sets up stdout and stdin IO handles.
handleIO :: [Flag] -> IO ()
handleIO opts = do
    case inputF opts of
        Just file -> do
            h <- openFile file ReadMode
            hDuplicateTo h stdin   -- h becomes the new stdin
        Nothing   -> return ()

    case outputF opts of
        Just file -> do
            h <- openFile file WriteMode
            hDuplicateTo h stdout  -- h becomes the new stdout
        Nothing   -> return ()

main :: IO ()
main = do
    (cmd, opts) <- getArgs >>= parse
    handleIO opts -- set up IO handles.
    case cmd of
        "compile"  -> runCompiler opts
        "sample"   -> runSampler opts
        "tune"     -> runTuner opts
        "spec"     -> runSpec opts
        _          -> fail' $ "[Error] Unrecognised command " ++ quote cmd ++ "."

-- | Reports system warnings.
sysWarnings :: System Int -> [Flag] -> IO ()
sysWarnings sys opts =
    case warnings sys of
        Left warn -> do
            hPrint stderr warn -- report the warnings
            when (Werror `elem` opts) $ exitWith (ExitFailure 1)

        _         -> return ()

-- | Performs semantic validity checks
--   returning the detected system type.
sysErrors :: System Int -> [Flag] -> IO SystemType
sysErrors sys opts =
    case errors (Force `elem` opts) sys of
        Left err   -> fail $ show err
        Right typ  -> return typ

-- | Prints parsing errors or returns the parsed system.
getSystem :: (ShowToken t, Ord t, ShowErrorComponent e)
          => Either (ParseError t e) a -> IO a

getSystem (Left err)  = printError err
getSystem (Right sys) = return sys

-- | Parses the system performing the necessary error
--   and warnings checks. Returns the parsed system and its type.
parseSystem :: [Flag] -> IO (System Int, SystemType)
parseSystem opts = do
    text <- getContents
    dat  <- parseSpec text
    sys  <- getSystem dat

    sysWarnings sys opts          -- check for warnings
    sysType <- sysErrors sys opts -- check for errors
    return (sys, sysType)

tuningConf :: System a -> (Double, Int)
tuningConf sys = (precision, maxiter)
    where arg       = T.defaultArgs sys
          ann       = annotations sys
          precision = withDouble ann "precision" 1.0e-9
          maxiter   = withInt ann "maxiter" (T.maxiters arg)

-- | Tunes the given system by either parsing the given paganini
--   data file corresponding to the given system, or by executing
--   the paganini tuner.
tuneSystem :: System Int
           -> [Flag] -> T.Parametrisation
           -> IO (PSystem Double)

tuneSystem sys opts prob =
    case tuningF opts of
        Nothing -> do
           let arg                  = T.defaultArgs sys
           let (precision, maxiter) = tuningConf sys
           dat <- T.runPaganini sys prob (Just $ arg { T.precision = precision
                                                     , T.maxiters  = maxiter
                                                     })
           getSystem dat
        Just file -> do
            dat  <- T.readPaganini sys prob file
            getSystem dat

compilerConf :: System a -> String
compilerConf sys = moduleName
    where ann        = annotations sys
          moduleName = withDefault ann "module" "Sampler"

-- | Runs the specification compiler.
runCompiler :: [Flag] -> IO ()
runCompiler opts = do
    (sys, sysType) <- parseSystem opts
    let moduleName = compilerConf sys

    tunedSystem    <- tuneSystem sys opts T.Cummulative
    case sysType of
        Rational  -> R.compile (config tunedSystem moduleName compilerTimestamp :: R.Conf)
        Algebraic -> A.compile (config tunedSystem moduleName compilerTimestamp :: A.Conf)
        _         -> fail "[Error] Unsupported system type."

samplerConf :: System a -> (Int, Int, String)
samplerConf sys = (lb, ub, gen)
    where ann = annotations sys
          lb  = withInt ann "lowerBound" 10
          ub  = withInt ann "upperBound" 200
          gen = withDefault ann "generate" (initType sys)

samplerErrors :: System a -> (Int, Int, String) -> IO ()
samplerErrors sys (lb, ub, genT) = do
    when (genT `S.notMember` types sys) (
        fail $ "[Error] " ++ genT ++ " does not name a type.")

    when (lb > ub) (fail "[Error] Lower bounds greater than the upper bound.")

-- | Runs the specification sampler.
runSampler :: [Flag] -> IO ()
runSampler opts = do
    (sys, _)           <- parseSystem opts
    let (lb, ub, genT) = samplerConf sys
    samplerErrors sys (lb, ub, genT)

    tunedSystem  <- tuneSystem sys opts T.Cummulative
    sample       <- sampleStrIO tunedSystem genT lb ub
    B.putStrLn   (encode sample)

runTuner :: [Flag] -> IO ()
runTuner opts = do
    (sys, _)    <- parseSystem opts
    tunedSystem <- tuneSystem sys opts T.Regular
    B.putStr $ encode (toSystemT $ system tunedSystem)

runSpec :: [Flag] -> IO ()
runSpec opts = do
    (sys, _) <- parseSystem opts
    T.writeSpecification sys stdout -- write specification to output
