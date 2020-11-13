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
  )
where

import           Prelude                 hiding ( fail )
import           Control.Monad                  ( unless )

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8    as B

import           GHC.IO.Handle

import           System.Console.GetOpt
import           System.Directory               ( doesFileExist )
import           System.Environment
import           System.Exit
import           System.IO

import           Text.Megaparsec         hiding ( parse )

import           Data.Boltzmann.Compiler
import qualified Data.Boltzmann.Compiler.Haskell.Algebraic
                                               as Algebraic

import           Data.Boltzmann.Internal.Annotations
import           Data.Boltzmann.Internal.Logging
import           Data.Boltzmann.Internal.Parser
import           Data.Boltzmann.Internal.TH     ( compileTime )
import qualified Data.Boltzmann.Internal.Tuner as Tuner
import           Data.Boltzmann.Internal.Utils
import           Data.Boltzmann.System
import           Data.Boltzmann.System.Errors
import           Data.Boltzmann.System.Parser
import           Data.Boltzmann.System.Tuner
import           Data.Boltzmann.System.Warnings

data Flag = InputFile  String  -- ^ input file location
          | OutputFile String  -- ^ output file location
          | Force              -- ^ whether to skip sanity checks
          | Werror             -- ^ whether to treat warnings as errors
          | Help               -- ^ whether to print usage help text
            deriving (Show,Eq)

options :: [OptDescr Flag]
options =
  [ Option "i"
           ["input"]
           (ReqArg InputFile "FILE")
           "Input specification file. If not given, STDIN is used instead."
  , Option "o"
           ["output"]
           (ReqArg OutputFile "FILE")
           "Output file. If not given, STDOUT is used instead."
  , Option "w"
           ["werror"]
           (NoArg Werror)
           "Whether to treat warnings as errors or not."
  , Option "f"
           ["force"]
           (NoArg Force)
           "Whether to skip input specification correctness checks."
  , Option "h" ["help"] (NoArg Help) "Prints this help message."
  ]

version :: String
version = "v2.0"

signature :: String
signature = "Boltzmann Brain " ++ version

versionHeader :: String
versionHeader = signature ++ " (c) 2017-2020."

-- | Available boltzmann-brain commands.
commands :: [(String, String)]
commands =
  [ ( "compile"
    , "Generates an analytic sampler corresponding to the given specification."
    )
  , ( "tune"
    , "Decorates the given specification with appropriate branching probabilities."
    )
  ]

-- | Renders the given commands and its description.
renderCmd :: (String -> String) -> (String, String) -> IO String
renderCmd pause (cmd, desc) = do
  cmd' <- bold cmd
  return (cmd' ++ pause cmd ++ desc)

-- | Renders a commands usage note.
commandsMsg :: IO String
commandsMsg =
  let cmdLen = 3 + maximum (map (length . fst) commands)
      offset x = cmdLen - length x        -- offest computing function
      pause x = replicate (offset x) ' '  -- treats offset as whitespace pause
  in  do
        cmds' <- underline "Commands:"
        xs    <- mapM (renderCmd pause) commands
        return (unlines $ [cmds', ""] ++ xs)

usageHeader :: IO String
usageHeader = do
  commandsMsg' <- commandsMsg
  usage'       <- underline "Usage:"
  return $ unlines
    [ artLogo
    , "" -- newline
    , commandsMsg'
    , usage' ++ " bb [COMMAND] [OPTIONS...]"
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
artLogo = unlines
  [ "                                                .--                             "
  , "                             .-`                ./:``  `                        "
  , "                             .:.-`      `.-` ..--:/    .+-           ``....:::. "
  , "                         ``.:`-:-::`   --..:--//:+/-````:-`.```.-`  `//::///---`"
  , "                       ``::-:` //+:.   `. `...`-::.++//++/:----:-:.-:/-`/oo+::/-"
  , "                      /o. .:::/-.-:`        `   -://:::/://       .::`.-..::/:- "
  , "            ``  ``````.-:`  `.`...-/`.         `- ``.`            `.:/:`  -:.   "
  , "            ...+-/-:--::-:`     -::/-/:-`.-.`..:::/+:.              `+`         "
  , "              `:-.-::::--::.```:/+:-:` ./+/:-+:. :-`.`              --.         "
  , "         ``       .`.:-``.////-``../++:/::-:/--::::-+/:.           -+-          "
  , "..`..-...:```````-:::`.`.///:-.`  ``-.-``.:.  `   `/.:/:`          `-`          "
  , "`:////::/+-/+/.`:/o++/:::++-:-.--        .    `     `.+/.           `           "
  , " `  :/:``--.:-  .--/+++-::-:-: `                     .:.                        "
  , " -::+:./:://+:--.-:://-``/+-`                        .`                         "
  , " -/`.-   -/o+/o+/+//+++//o+.                                                    "
  , "  `    `:/+/s+-...```.://--                                                     "
  , logoHeader
  , buildTimeHeader
  ]

inputF :: [Flag] -> Maybe String
inputF (InputFile f : _ ) = Just f
inputF (_           : fs) = inputF fs
inputF []                 = Nothing

outputF :: [Flag] -> Maybe String
outputF (OutputFile f : _ ) = Just f
outputF (_            : fs) = outputF fs
outputF []                  = Nothing

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
parse argv = case getOpt Permute options argv of
  (opts, cmds, [])
    | Help `elem` opts -> usage
    | null cmds        -> failWithUsage "Expected a single command."
    | length cmds /= 1 -> failWithUsage "Expected a single command."
    | otherwise        -> return (head cmds, opts)

  (_, _, errs) -> failWithUsage $ concat errs

-- | Sets up stdout and stdin IO handles.
handleIO :: [Flag] -> IO ()
handleIO opts = do
  case inputF opts of
    Just file -> do
      inputExists <- doesFileExist file
      unless inputExists
             (fail' $ "Input file " ++ quote file ++ " does not exist.")

      h <- openFile file ReadMode
      hDuplicateTo h stdin   -- h becomes the new stdin
    Nothing -> return ()

  case outputF opts of
    Just file -> do
      h <- openFile file WriteMode
      hDuplicateTo h stdout  -- h becomes the new stdout
    Nothing -> return ()

main :: IO ()
main = do
  (cmd, opts) <- getArgs >>= parse
  handleIO opts -- set up IO handles.
  case cmd of
    "compile" -> runCompiler opts
    "tune"    -> runTuner opts
    _         -> do
      err <- unrecognisedCmd cmd
      failWithUsage err

unrecognisedCmd :: String -> IO String
unrecognisedCmd cmd = do
  let cmd' = quote cmd
  cmdHint <- bold $ closest (map fst commands) cmd
  return
    $  "Unrecognised command "
    ++ cmd'
    ++ ". Did you mean "
    ++ cmdHint
    ++ "?"

-- | Prints parsing errors or returns the parsed system.
getSystem
  :: (Stream t, ShowErrorComponent e) => Either (ParseErrorBundle t e) a -> IO a

getSystem (Left  err) = printError err
getSystem (Right sys) = return sys

-- | Parses the system performing the necessary error
--   and warnings checks. Returns the parsed system and its type.
parseSystem :: [Flag] -> IO (System Int, SystemType)
parseSystem opts = do
  info "Parsing system..."
  text <- getContents
  dat  <- parseSpec text
  sys  <- getSystem dat

  let force = Force `elem` opts
  sysType <- errors force sys       -- check for errors

  let werror = Werror `elem` opts
  warnings werror force sys         -- check for warnings
  return (sys, sysType)

-- | Tunes the given system by either parsing the given paganini
--   data file corresponding to the given system, or by executing
--   the paganini tuner.
tuneSystem :: System Int -> Tuner.Parametrisation -> IO (PSystem Double)

tuneSystem sys prob = do
  dat <- runPaganini sys prob
  getSystem dat

compilerConf :: System a -> String
compilerConf sys = moduleName
 where
  ann        = annotations sys
  moduleName = withString ann "module" "Sampler"

-- | Runs the specification compiler.
runCompiler :: [Flag] -> IO ()
runCompiler opts = do
  (sys, _) <- parseSystem opts
  let moduleName = compilerConf sys

  tunedSystem <- tuneSystem sys Tuner.Regular
  info "Running sampler compiler..."

  compile (config tunedSystem moduleName compilerTimestamp :: Algebraic.Conf)

runTuner :: [Flag] -> IO ()
runTuner opts = do
  (sys, _)    <- parseSystem opts
  tunedSystem <- tuneSystem sys Tuner.Regular
  B.putStr $ encode (toSystemT $ system tunedSystem)
