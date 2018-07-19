{-|
 Module      : Main
 Description : Boltzmann brain executable.
 Copyright   : (c) Maciej Bendkowski, 2017-2018

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
import qualified Data.Text.Lazy.IO as T

import Text.Megaparsec hiding (parse)
import qualified Data.Set as S

import Data.Boltzmann.System
import Data.Boltzmann.System.Parser
import Data.Boltzmann.System.Errors
import Data.Boltzmann.System.Warnings
import Data.Boltzmann.System.Sampler
import Data.Boltzmann.System.Renderer

import Data.Boltzmann.Internal.Parser
import Data.Boltzmann.Internal.Annotations
import Data.Boltzmann.Internal.Logging
import Data.Boltzmann.Internal.Utils

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

version :: String
version = "v1.3.1.3"

signature :: String
signature = "Boltzmann Brain " ++ version

versionHeader :: String
versionHeader = signature ++ " (c) Maciej Bendkowski and Sergey Dovgal 2017-2018"

-- | Available boltzmann-brain commands.
commands :: [(String, String)]
commands = [("compile","Generates a Boltzmann sampler corresponding to the given specification.")
           ,("sample","Generates a random structure corresponding to the given specification.")
           ,("render","Generates a random structure and outputs its dotfile representation.")
           ,("tune","Decorates the given specification with appropriate Boltzmann branching probabilities.")
           ,("spec","Generates a paganini input tuning problem corresponding to the given specification.")
           ]

-- | Renders the given commands and its description.
renderCmd :: (String -> String) -> (String, String) -> IO String
renderCmd pause (cmd,desc) = do
    cmd'  <- bold cmd
    return (cmd' ++ pause cmd ++ desc)

-- | Renders a commands usage note.
commandsMsg :: IO String
commandsMsg =
    let cmdLen   = 3 + maximum (map (length . fst) commands)
        offset x = cmdLen - length x         -- offest computing function
        pause  x = replicate (offset x) ' '  -- treats offset as whitespace pause
        in do
            cmds' <- underline "Commands:"
            xs <- mapM (renderCmd pause) commands
            return (unlines $ [cmds', ""] ++ xs)

usageHeader :: IO String
usageHeader = do
    commandsMsg' <- commandsMsg
    usage' <- underline "Usage:"
    return $ unlines [versionHeader, ""
                     ,commandsMsg'
                     ,usage' ++ " bb [COMMAND] [OPTIONS...]"
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

-- | Logs an error and exists with the usage info.
failWithUsage :: String -> IO a
failWithUsage m = do
    let msg' = ensureLn m
    usage' <- usageHeader
    fail' (msg' ++ usageInfo usage' options)

-- | Prints the usage info and exists.
usage :: IO a
usage = do
    usage' <- usageHeader
    putStrLn $ usageInfo usage' options
    exitSuccess

-- | Parses the cli arguments into the command string
--   and some additional (optional) flags.
parse :: [String] -> IO (String, [Flag])
parse argv =
    case getOpt Permute options argv of
        (opts, cmds, [])
            | null cmds        -> usage
            | Help `elem` opts -> usage
            | length cmds /= 1 -> failWithUsage "Expected a single command."
            | otherwise        -> return (head cmds, opts)

        (_, _, errs) -> failWithUsage $ concat errs

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
        "render"   -> runRenderer opts
        "tune"     -> runTuner opts
        "spec"     -> runSpec opts
        _          -> do
           err <- unrecognisedCmd cmd
           failWithUsage err

unrecognisedCmd :: String -> IO String
unrecognisedCmd cmd = do
    let cmd' = quote cmd
    cmdHint <- bold $ closest (map fst commands) cmd
    return $ "Unrecognised command " ++ cmd' ++ ". Did you meant " ++ cmdHint ++ "?"

-- | Reports system warnings.
sysWarnings :: System Int -> [Flag] -> IO ()
sysWarnings sys opts =
    case warnings sys of
        Left warnMsg -> do
            let f = if Werror `elem` opts then warn' else warn
            f (show warnMsg)
        _         -> return ()

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

    sysWarnings sys opts        -- check for warnings

    let force = Force `elem` opts
    sysType <- errors force sys -- check for errors
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
        _         -> fail "Unsupported system type."

samplerConf :: System a -> (Int, Int, String)
samplerConf sys = (lb, ub, gen)
    where ann = annotations sys
          lb  = withInt ann "lowerBound" 10
          ub  = withInt ann "upperBound" 200
          gen = withDefault ann "generate" (initType sys)

samplerErrors :: System a -> (Int, Int, String) -> IO ()
samplerErrors sys (lb, ub, genT) = do
    when (genT `S.notMember` types sys) (
        fail $ genT ++ " does not name a type.")

    when (lb > ub) (fail "Lower bounds greater than the upper bound.")

getSample :: System Int -> [Flag] -> IO Structure
getSample sys opts = do
    let (lb, ub, genT) = samplerConf sys
    samplerErrors sys (lb, ub, genT)

    tunedSystem  <- tuneSystem sys opts T.Cummulative
    sampleStrIO tunedSystem genT lb ub

-- | Runs the specification sampler.
runSampler :: [Flag] -> IO ()
runSampler opts = do
    (sys, _) <- parseSystem opts
    sample   <- getSample sys opts
    B.putStrLn $ encode sample

rendererConf :: System a -> [Flag]
             ->  IO ColorScheme

rendererConf sys opts =
        let ann = annotations sys
            col = withString ann "colorScheme" "random"
            in case col of
                   "black" -> return BlackCol
                   _       -> do
                        let f = if Werror `elem` opts then warn' else warn
                        f "Using default random color scheme."
                        return RandomCol

-- | Runs the structure renderer.
runRenderer :: [Flag] -> IO ()
runRenderer opts = do
    (sys, _) <- parseSystem opts
    cs       <- rendererConf sys opts

    sample  <- getSample sys opts
    dotfile <- toDotFile cs sample
    T.putStrLn dotfile

runTuner :: [Flag] -> IO ()
runTuner opts = do
    (sys, _)    <- parseSystem opts
    tunedSystem <- tuneSystem sys opts T.Regular
    B.putStr $ encode (toSystemT $ system tunedSystem)

runSpec :: [Flag] -> IO ()
runSpec opts = do
    (sys, _) <- parseSystem opts
    T.writeSpecification sys stdout -- write specification to output
