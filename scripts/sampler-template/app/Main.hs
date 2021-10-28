module Main where

import           GHC.IO.Handle

import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           Data.Aeson

import           Sampler
import           Control.Monad                  ( replicateM )

import qualified Data.ByteString.Lazy.Char8    as B

data Flag = OutputFile String  -- ^ output file location
          | LowerBound String  -- ^ structure size lower bound
          | UpperBound String  -- ^ structure size upper bound
          | Samples    String  -- ^ number of samples
          | Help               -- ^ whether to print usage help text
            deriving (Show,Eq)

options :: [OptDescr Flag]
options =
  [ Option "o"
           ["output"]
           (ReqArg OutputFile "FILE")
           "Output file. If not given, STDOUT is used instead."
  , Option "l"
           ["lower-bound"]
           (ReqArg LowerBound "N")
           "Lower bound for the structure size. Default: 10."
  , Option "u"
           ["upper-bound"]
           (ReqArg UpperBound "N")
           "Upper bound for the structure size. Default: 50."
  , Option "s"
           ["samples"]
           (ReqArg Samples "N")
           "Number of structures to sample. Default: 1."
  , Option "h" ["help"] (NoArg Help) "Prints this help message."
  ]

outputF :: [Flag] -> Maybe String
outputF (OutputFile f : _ ) = Just f
outputF (_            : fs) = outputF fs
outputF []                  = Nothing

lowerBound :: [Flag] -> Int
lowerBound (LowerBound n : _   ) = (read n)
lowerBound (_            : opts) = lowerBound opts
lowerBound []                    = 10

upperBound :: [Flag] -> Int
upperBound (UpperBound n : _   ) = read n
upperBound (_            : opts) = upperBound opts
upperBound []                    = 50

samples :: [Flag] -> Int
samples (Samples n : _   ) = read n
samples (_         : opts) = samples opts
samples []                 = 1

fail' s = do
  hPutStrLn stderr s
  exitWith (ExitFailure 1)


align :: String -> String -> String
align s t = s ++ t ++ replicate (80 - length s - length t) ' '

version :: String
version = "v2.0"

signature :: String
signature = "Boltzmann Brain " ++ version

versionHeader :: String
versionHeader = signature ++ " (c) 2017-2021."

logoHeader :: String
logoHeader = align s versionHeader
  where s = "       `/:/::-  `  ././:.`             "

buildTimeHeader :: String
buildTimeHeader = align s "Sampler"
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

usage = do
  putStrLn artLogo
  putStr (usageInfo "Usage:" options)
  exitWith (ExitFailure 1)

-- | Parses the CLI arguments into the command string
--   and some additional (optional) flags.
parse :: [String] -> IO [Flag]
parse argv = case getOpt Permute options argv of
  (opts, _, []) | Help `elem` opts -> usage
                | otherwise        -> return opts

  (_, _, errs) -> fail' $ concat errs

-- | Sets up stdout and stdin IO handles.
handleIO :: [Flag] -> IO ()
handleIO opts = do
  case outputF opts of
    Just file -> do
      h <- openFile file WriteMode
      hDuplicateTo h stdout  -- h becomes the new stdout
    Nothing -> return ()

main :: IO ()
main = do
  opts <- getArgs >>= parse
  handleIO opts -- set up IO handles.

  let lb = lowerBound opts
  let ub = upperBound opts
  let n  = samples opts

  xs <- replicateM n (SAMPLE_COMMAND lb ub)
  B.putStrLn $ encode xs
