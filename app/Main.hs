{-|
 Module      : Main
 Description : Boltzmann brain executable.
 Copyright   : (c) Maciej Bendkowski, 2017

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Main
    ( main
    ) where

import System.IO
import System.Exit
import System.Console.GetOpt
import System.Environment

import Data.Maybe (fromMaybe)
import Data.List (nub)
import Numeric

import Data.Boltzmann.System
import Data.Boltzmann.System.Parser
import Data.Boltzmann.System.Errors
import Data.Boltzmann.Internal.ParserUtils

import qualified Data.Boltzmann.System.Tuner as T
import qualified Data.Boltzmann.Compiler.Haskell.Algebraic as A
import qualified Data.Boltzmann.Compiler.Haskell.Rational as R

data Flag = Precision String
          | MaxIter String
          | ModuleName String
          | InputPaganini String
          | OutputPaganini
          | WithLists
          | WithShow
          | WithIO
          | Force
          | Version
          | Help
            deriving (Eq)

options :: [OptDescr Flag]
options = [Option "p" ["precision"] (ReqArg Precision "p")
            "Tuning precision. Defaults to 1.0e-9.",

          Option "e" ["maxiter"] (ReqArg MaxIter "e")
            "Maximum number of tuner iterations.",

          Option "g" ["to-paganini"] (NoArg OutputPaganini)
            "Output a suitable Paganini specification for the given system.",

          Option "t" ["from-paganini"] (ReqArg InputPaganini "t")
            "Input a suitable Paganini tuning vector for the given system.",

           Option "m" ["module"] (ReqArg ModuleName "m")
            "The resulting Haskell module name. Defaults to Sampler.",

           Option "i" ["with-io"] (NoArg WithIO)
            "Whether to generate IO actions for the samplers.",

           Option "l" ["with-lists"] (NoArg WithLists)
            "Whether to generate lists samplers for all types.",

           Option "d" ["with-show"] (NoArg WithShow)
            "Whether to generate data types deriving Show.",

           Option "f" ["force"] (NoArg Force)
            "Whether to skip the well-foundedness check.",

           Option "v" ["version"] (NoArg Version)
            "Prints the program version number.",

           Option "h?" ["help"] (NoArg Help)
            "Prints this help message."]

usageHeader :: String
usageHeader = "Usage: bb [OPTIONS...]"

versionHeader :: String
versionHeader = "Boltzmann brain v1.2 (c) Maciej Bendkowski 2017"

compilerTimestamp :: String
compilerTimestamp = "Boltzmann brain v1.2"

parseFloating :: String -> Rational
parseFloating s = (fst $ head (readFloat s)) :: Rational

getPrecision :: [Flag] -> Double
getPrecision (Precision eps : _) = fromRational $ parseFloating eps
getPrecision (_:fs)              = getPrecision fs
getPrecision []                  = fromRational 1.0e-9

getModuleName :: [Flag] -> String
getModuleName (ModuleName name : _) = name
getModuleName (_:fs)                = getModuleName fs
getModuleName []                    = "Sampler"

getMaxIter :: [Flag] -> Maybe Int
getMaxIter (MaxIter iter : _) = Just $ read iter
getMaxIter (_:fs)             = getMaxIter fs
getMaxIter []                 = Nothing

useIO :: [Flag] -> Bool
useIO flags = WithIO `elem` flags

genLists :: [Flag] -> Bool
genLists flags = WithLists `elem` flags

genShow :: [Flag] -> Bool
genShow flags = WithShow `elem` flags

toPaganini :: [Flag] -> Bool
toPaganini flags = OutputPaganini `elem` flags

useForce :: [Flag] -> Bool
useForce flags = Force `elem` flags

fromPaganini :: [Flag] -> Maybe String
fromPaganini (InputPaganini s : _) = Just s
fromPaganini (_:fs)                = fromPaganini fs
fromPaganini []                    = Nothing

parse :: [String] -> IO ([Flag], [String])
parse argv = case getOpt Permute options argv of
               (ops, nonops, [])
                    | Help `elem` ops -> do
                        putStr $ usageInfo usageHeader options
                        exitSuccess
                    | Version `elem` ops -> do
                        putStrLn versionHeader
                        exitSuccess
                    | otherwise -> return (nub (concatMap mkset ops), fs)
                        where
                            fs = if null nonops then [] else nonops
                            mkset x = [x]
               (_, _, errs) -> do
                    hPutStr stderr (concat errs ++ usageInfo usageHeader options)
                    exitWith (ExitFailure 1)

run :: [Flag]
    -> String
    -> IO ()

run flags f = do
    sys <- parseSystem f
    case sys of
      Left err   -> printError err
      Right sys' -> case errors (useForce flags) sys' of
                      Left err'  -> reportSystemError err'
                      Right sysT -> if toPaganini flags then T.writeSpecification sys' stdout
                                                        else runCompiler sys' sysT flags

confCompiler :: PSystem Double -> SystemType -> [Flag] -> IO ()
confCompiler sys Rational flags = do
    let conf = R.Conf { R.paramSys    = sys
                      , R.moduleName  = getModuleName flags
                      , R.compileNote = compilerTimestamp
                      , R.withIO      = useIO flags
                      , R.withShow    = genShow flags
                      }
    R.compile conf

confCompiler sys Algebraic flags = do
    let conf = A.Conf { A.paramSys    = sys
                      , A.moduleName  = getModuleName flags
                      , A.compileNote = compilerTimestamp
                      , A.withIO      = useIO flags
                      , A.withLists   = genLists flags
                      , A.withShow    = genShow flags
                      }
    A.compile conf

confCompiler _ _ _ = error "I wasn't expecting the Spanish inquisition!"

runCompiler :: System Int -> SystemType -> [Flag] -> IO ()
runCompiler sys sysT flags =
    case fromPaganini flags of
      Nothing -> do
          let arg = T.defaultArgs sys
          pag <- T.runPaganini sys (Just $ arg { T.precision = getPrecision flags
                                               , T.maxiters  = fromMaybe (T.maxiters arg)
                                                                         (getMaxIter flags) })
          case pag of
            Left err   -> printError err
            Right sys' -> confCompiler sys' sysT flags
      Just s  -> do
          pag <- T.readPaganini sys s
          case pag of
            Left err   -> printError err
            Right sys' -> confCompiler sys' sysT flags

reportSystemError :: SystemError -> IO ()
reportSystemError err = do
    hPrint stderr err
    exitWith (ExitFailure 1)

main :: IO ()
main = do
    (ops, fs) <- getArgs >>= parse
    case fs of
      []     -> exitSuccess
      (f:_)  -> do run ops f
                   exitSuccess
