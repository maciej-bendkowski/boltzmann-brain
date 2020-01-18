{-|
 Module      : Main
 Description : Boltzmann brain executable.
 Copyright   : (c) Maciej Bendkowski, 2017-2020

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
{-# LANGUAGE TemplateHaskell #-}
module Main
    ( main
    ) where

import Prelude hiding (fail)
import Control.Monad (replicateM, unless)

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy.IO as T

import GHC.IO.Handle

import System.Console.GetOpt
import System.Directory (doesFileExist)
import System.Environment
import System.Exit
import System.FilePath (takeExtension)
import System.IO

import Text.Megaparsec hiding (parse)

import Data.Boltzmann.Compiler
import qualified Data.Boltzmann.Compiler.Haskell.Algebraic as A
import qualified Data.Boltzmann.Compiler.Haskell.Rational as R
import Data.Boltzmann.Internal.Annotations
import Data.Boltzmann.Internal.Logging
import Data.Boltzmann.Internal.Parser
import Data.Boltzmann.Internal.TH (compileTime)
import qualified Data.Boltzmann.Internal.Tuner as T
import Data.Boltzmann.Internal.Utils
import Data.Boltzmann.System
import Data.Boltzmann.System.Errors
import Data.Boltzmann.System.Parser
import Data.Boltzmann.System.Renderer
import Data.Boltzmann.System.Sampler
import Data.Boltzmann.System.Tuner
import qualified Data.Boltzmann.System.Tuner.Algebraic as TA
import qualified Data.Boltzmann.System.Tuner.Rational as TR
import Data.Boltzmann.System.Warnings

data Flag = InputFile  String  -- ^ input file location
          | OutputFile String  -- ^ output file location
          | TuningFile String  -- ^ paganini tuning data file location
          | Format String      -- ^ whether to assume a rational input specification format
          | Force              -- ^ whether to skip sanity checks
          | Werror             -- ^ whether to treat warnings as errors
          | Help               -- ^ whether to print usage help text
            deriving (Show,Eq)

options :: [OptDescr Flag]
options = [Option "i" ["input"] (ReqArg InputFile "FILE")
            "Input specification file. If not given, STDIN is used instead.",

           Option "o" ["output"] (ReqArg OutputFile "FILE")
            "Output file. If not given, STDOUT is used instead.",

           Option "t" ["tuning-data"] (ReqArg TuningFile "FILE")
            "Tuning data file created using paganini.",

           Option "r" ["format"] (ReqArg Format "EXT")
            "Input format (algebraic|rational). Default: algebaic.",

           Option "w" ["werror"] (NoArg Werror)
            "Whether to treat warnings as errors or not.",

           Option "f" ["force"] (NoArg Force)
            "Whether to skip input specification correctness checks.",

           Option "h?" ["help"] (NoArg Help)
            "Prints this help message."]

version :: String
version = "v1.6"

signature :: String
signature = "Boltzmann Brain " ++ version

versionHeader :: String
versionHeader = signature ++ " (c) 2017-2020."

-- | Available boltzmann-brain commands.
commands :: [(String, String)]
commands =
    [("compile", "Generates an analytic sampler corresponding to the given specification.")
    ,("sample" , "Generates a random structure corresponding to the given specification.")
    ,("render" , "Generates a random structure and outputs its dotfile representation.")
    ,("tune"   , "Decorates the given specification with appropriate branching probabilities.")
    ,("spec"   , "Generates a corresponding paganini input tuning problem.")
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
    return $ unlines [artLogo
                     ,"" -- newline
                     ,commandsMsg'
                     ,usage' ++ " bb [COMMAND] [OPTIONS...]"
                     ]

compilerTimestamp :: String
compilerTimestamp = signature ++ " (" ++ $compileTime ++ ")"

compilerBuild :: String
compilerBuild = "Build time: " ++ $compileTime ++ "." -- Note: computed at compilation

align :: String -> String -> String
align s t = s ++ t ++ replicate (80 - length s - length t) ' '

logoHeader :: String
logoHeader = align s versionHeader
    where s = "       `/:/::-  `  ././:.`             "

buildTimeHeader :: String
buildTimeHeader = align s compilerBuild
    where s = "        .-         `..-                "

artLogo :: String
artLogo =
    unlines
       ["                                                .--                             ",
        "                             .-`                ./:``  `                        ",
        "                             .:.-`      `.-` ..--:/    .+-           ``....:::. ",
        "                         ``.:`-:-::`   --..:--//:+/-````:-`.```.-`  `//::///---`",
        "                       ``::-:` //+:.   `. `...`-::.++//++/:----:-:.-:/-`/oo+::/-",
        "                      /o. .:::/-.-:`        `   -://:::/://       .::`.-..::/:- ",
        "            ``  ``````.-:`  `.`...-/`.         `- ``.`            `.:/:`  -:.   ",
        "            ...+-/-:--::-:`     -::/-/:-`.-.`..:::/+:.              `+`         ",
        "              `:-.-::::--::.```:/+:-:` ./+/:-+:. :-`.`              --.         ",
        "         ``       .`.:-``.////-``../++:/::-:/--::::-+/:.           -+-          ",
        "..`..-...:```````-:::`.`.///:-.`  ``-.-``.:.  `   `/.:/:`          `-`          ",
        "`:////::/+-/+/.`:/o++/:::++-:-.--        .    `     `.+/.           `           ",
        " `  :/:``--.:-  .--/+++-::-:-: `                     .:.                        ",
        " -::+:./:://+:--.-:://-``/+-`                        .`                         ",
        " -/`.-   -/o+/o+/+//+++//o+.                                                    ",
        "  `    `:/+/s+-...```.://--                                                     ",
                                                       logoHeader                         ,
                                                       buildTimeHeader                    ]

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

format :: [Flag] -> Maybe Format
format (Format "algebraic" : _) = Just AlgebraicF
format (Format "rational" : _)  = Just RationalF
format (_:fs)                   = format fs
format []                       = Nothing

-- | Logs an error and exists with the usage info.
failWithUsage :: String -> IO a
failWithUsage m = do
    usage' <- usageHeader
    putStrLn $ usageInfo usage' options
    fail' m

-- | Prints the usage info and exists.
usage :: IO a
usage = do
    usage' <- usageHeader
    putStr $ usageInfo usage' options
    exitSuccess

-- | Parses the CLI arguments into the command string
--   and some additional (optional) flags.
parse :: [String] -> IO (String, [Flag])
parse argv =
    case getOpt Permute options argv of
        (opts, cmds, [])
            | Help `elem` opts -> usage
            | null cmds        -> failWithUsage "Expected a single command."
            | length cmds /= 1 -> failWithUsage "Expected a single command."
            | otherwise        -> return (head cmds, opts)

        (_, _, errs) -> failWithUsage $ concat errs

parseFileExt :: FilePath -> Maybe Format
parseFileExt file =
    case takeExtension file of
        ".rat" -> Just RationalF
        ".alg" -> Just AlgebraicF
        _      -> Nothing

inputFormat :: [Flag] -> FilePath -> IO Format
inputFormat opts file = case format opts of
    Just f -> return f
    Nothing ->
        case parseFileExt file of
           Nothing       -> do
              warn $ "Cannot guess input specification format."
                         ++ " Defaulting to 'algebraic'."
              hint $ "Use conventional file extensions"
                         ++ " '.rat', '.alg' or use the --format flag."
              return AlgebraicF
           Just inFormat -> return inFormat

getInputFormat :: [Flag] -> IO Format
getInputFormat opts =
    case inputF opts of
        Just file -> inputFormat opts file
        Nothing   -> case format opts of
                         Just f  -> return f
                         Nothing -> return AlgebraicF -- default

-- | Sets up stdout and stdin IO handles.
handleIO :: [Flag] -> IO ()
handleIO opts = do
    case inputF opts of
        Just file -> do
            inputExists <- doesFileExist file
            unless inputExists
                (fail' $ "Input file " ++  quote file ++ " does not exist.")

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
    return $ "Unrecognised command " ++ cmd' ++
                ". Did you mean " ++ cmdHint ++ "?"

-- | Prints parsing errors or returns the parsed system.
getSystem :: (Stream t, ShowErrorComponent e)
          => Either (ParseErrorBundle t e) a -> IO a

getSystem (Left err)  = printError err
getSystem (Right sys) = return sys

-- | Parses the system performing the necessary error
--   and warnings checks. Returns the parsed system and its type.
parseSystem :: [Flag] -> IO (System Int, SystemType)
parseSystem opts = do
    inFormat <- getInputFormat opts
    info $ "Using "++ quote (show inFormat) ++ " specification format."

    info "Parsing system..."
    text <- getContents
    dat  <- parseSpec inFormat text
    sys  <- getSystem dat

    let force = Force `elem` opts
    sysType <- errors inFormat force sys -- check for errors

    let werror = Werror `elem` opts
    warnings werror force sys         -- check for warnings

    return (sys, sysType)

tuningConf :: System a -> (Double, Int)
tuningConf sys = (precision, maxiter)
    where arg       = T.defaultArgs sys
          ann       = annotations sys
          precision = withDouble ann "precision" 1.0e-20
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
           sysFormat <- getInputFormat opts
           dat <- runPaganini sysFormat sys prob
                    (Just $ arg { T.precision = precision
                                , T.maxiters  = maxiter
                                })
           getSystem dat
        Just file -> do
            sysFormat <- getInputFormat opts
            dat  <- readPaganini sysFormat sys prob file
            getSystem dat

compilerConf :: System a -> String
compilerConf sys = moduleName
    where ann        = annotations sys
          moduleName = withString ann "module" "Sampler"

-- | Runs the specification compiler.
runCompiler :: [Flag] -> IO ()
runCompiler opts = do
    (sys, _) <- parseSystem opts
    let moduleName = compilerConf sys

    tunedSystem    <- tuneSystem sys opts T.Regular
    info "Running sampler compiler..."

    sysFormat <- getInputFormat opts
    case sysFormat of
        RationalF  -> R.compile (config tunedSystem moduleName
                                 compilerTimestamp :: R.Conf)
        AlgebraicF -> A.compile (config tunedSystem moduleName
                                 compilerTimestamp :: A.Conf)

samplerConf :: System Int -> [Flag] -> IO (Int, Int, Int, String)
samplerConf sys opts =
    let ann = annotations sys
        n   = withInt ann "samples" 1
        lb  = withInt ann "lowerBound" 10
        ub  = withInt ann "upperBound" 200
        gen = withString ann "generate" (initType sys)
        in case "samples" `M.lookup` ann of
           Just _  -> return (lb, ub, n, gen)
           Nothing -> do
               let f = if Werror `elem` opts then warn' else warn
               f "No explicit @samples annotation. Sampling a single structure."
               return (lb, ub, n, gen)

getSamples :: System Int -> [Flag] -> IO [Structure]
getSamples sys opts = do
    (lb, ub, n, genT) <- samplerConf sys opts
    -- TODO: Consider having generic method of generating samples
    --       reusing the compiler code. Ad-hoc compilation?
    tunedSystem       <- tuneSystem sys opts T.Cummulative
    replicateM n (sampleStrIO tunedSystem genT lb ub) -- get n samples.

-- | Runs the specification sampler.
runSampler :: [Flag] -> IO ()
runSampler opts = do
    (sys, _) <- parseSystem opts
    info "Sampling random structure..."
    samples <- getSamples sys opts
    B.putStrLn $ encode samples

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

    -- generate a single structure
    samples  <- getSamples sys opts

    info "Writing dotfile output..."

    -- TODO: Consider rendering multiple structures.
    dotfile <- toDotFile cs (head samples)
    T.putStrLn dotfile

runTuner :: [Flag] -> IO ()
runTuner opts = do
    (sys, _)    <- parseSystem opts
    tunedSystem <- tuneSystem sys opts T.Regular
    B.putStr $ encode (toSystemT $ system tunedSystem)

runSpec :: [Flag] -> IO ()
runSpec opts = do
    (sys, _) <- parseSystem opts
    sysFormat <- getInputFormat opts
    -- write specification to output
    case sysFormat of
        RationalF  -> TR.writeSpecification sys stdout
        AlgebraicF -> TA.writeSpecification sys stdout
