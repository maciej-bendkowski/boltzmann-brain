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

import qualified Data.Map as M

import Data.Boltzmann.System
import Data.Boltzmann.System.Parser
import Data.Boltzmann.System.Errors
import Data.Boltzmann.Internal.Parser

import qualified Data.Boltzmann.System.Tuner as T

import Data.Boltzmann.Compiler
import qualified Data.Boltzmann.Compiler.Haskell.Algebraic as A
import qualified Data.Boltzmann.Compiler.Haskell.Rational as R

data Flag = OutputFile String
          | InputPaganini String
          | OutputPaganini
          | Force
          | Version
          | Help
            deriving (Eq)

options :: [OptDescr Flag]
options = [Option "o" ["output"] (ReqArg OutputFile "FILE")
            "Optional output file.",

           Option "p" ["paganini-in"] (ReqArg InputPaganini "FILE")
            "Paganini tuning vector for the given system.",

           Option "s" ["paganini-out"] (NoArg OutputPaganini)
            "Output a suitable Paganini specification for the given system.",

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

getPrecision :: System a -> Double
getPrecision sys =
    case "precision" `M.lookup` annotations sys of
      Just x  -> read x :: Double
      Nothing -> 1.0e-9

getMaxIter :: System a -> Maybe Int
getMaxIter sys =
    case "maxiter" `M.lookup` annotations sys of
      Just x  -> return (read x :: Int)
      Nothing -> Nothing

getModuleName :: System a -> String
getModuleName sys = fromMaybe "Sampler" ("module" `M.lookup` annotations sys)

output :: [Flag] -> Maybe String
output (OutputFile f : _) = Just f
output (_:fs)             = output fs
output []                 = Nothing

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
                      Right sysT -> if toPaganini flags then writeSpec sys' (output flags)
                                                        else runCompiler sys' sysT flags

writeSpec :: System Int -> Maybe FilePath -> IO ()
writeSpec sys' (Just f) = withFile f WriteMode (T.writeSpecification sys')
writeSpec sys' Nothing  = T.writeSpecification sys' stdout

confCompiler :: PSystem Double -> Maybe String -> SystemType -> IO ()
confCompiler sys outputFile Rational = do
    let conf = config sys outputFile
            (getModuleName $ system sys)
            compilerTimestamp :: R.Conf
    R.compile conf

confCompiler sys outputFile Algebraic = do
    let conf = config sys outputFile
            (getModuleName $ system sys)
            compilerTimestamp :: A.Conf
    A.compile conf

confCompiler _ _ _ = error "I wasn't expecting the Spanish inquisition!"

runCompiler :: System Int -> SystemType -> [Flag] -> IO ()
runCompiler sys sysT flags =
    case fromPaganini flags of
      Nothing -> do
          let arg = T.defaultArgs sys
          pag <- T.runPaganini sys (Just $ arg { T.precision = getPrecision sys
                                               , T.maxiters  = fromMaybe (T.maxiters arg)
                                                                         (getMaxIter sys) })
          case pag of
            Left err   -> printError err
            Right sys' -> confCompiler sys' (output flags) sysT
      Just s  -> do
          pag <- T.readPaganini sys s
          case pag of
            Left err   -> printError err
            Right sys' -> confCompiler sys' (output flags) sysT

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
