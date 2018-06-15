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

import System.IO
import System.Exit
import System.Console.GetOpt
import System.Environment

import Control.Monad (when)
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import Data.List (nub)

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

import Text.Megaparsec hiding (parse)

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Boltzmann.System
import Data.Boltzmann.System.Parser
import Data.Boltzmann.System.Errors
import Data.Boltzmann.System.Warnings
import Data.Boltzmann.System.Sampler
import Data.Boltzmann.Internal.Parser

import qualified Data.Boltzmann.System.Tuner as T

import Data.Boltzmann.Compiler
import qualified Data.Boltzmann.Compiler.Haskell.Algebraic as A
import qualified Data.Boltzmann.Compiler.Haskell.Rational as R

data Flag = OutputFile String
          | InputPaganini String
          | OutputPaganini
          | Generate
          | Force
          | Werror
          | Tune
          | Stdin
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

           Option "w" ["werror"] (NoArg Werror)
            "Whether to treat warnings as errors.",

           Option "f" ["force"] (NoArg Force)
            "Whether to skip the well-foundedness check.",

           Option "t" ["tune"] (NoArg Tune)
            "Whether to output a textual representation of the tuned system instead.",

           Option "g" ["generate"] (NoArg Generate)
            "Generates a random structure instead of compiling a sampler.",

           Option "i" ["stdin"] (NoArg Stdin)
            "Reads the input specification from the stdin instead of given input file.",

           Option "v" ["version"] (NoArg Version)
            "Prints the program version number.",

           Option "h?" ["help"] (NoArg Help)
            "Prints this help message."]

usageHeader :: String
usageHeader = "Usage: bb [OPTIONS...]"

versionHeader :: String
versionHeader = "Boltzmann Brain v1.3.1.3 (c) Maciej Bendkowski and Sergey Dovgal 2018"

compilerTimestamp :: String
compilerTimestamp = "Boltzmann Brain v1.3.1.3"

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

getStrLowerBound :: System a -> Int
getStrLowerBound sys =
    case "lowerBound" `M.lookup` annotations sys of
      Just x  -> read x :: Int
      Nothing -> 10

getStrUpperBound :: System a -> Int
getStrUpperBound sys =
    case "upperBound" `M.lookup` annotations sys of
      Just x  -> read x :: Int
      Nothing -> 200

getGenType :: System a -> String
getGenType sys =
        fromMaybe (initType sys) ("generate" `M.lookup` annotations sys)

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
    -> Maybe String
    -> IO ()

run flags (Just f) = do
       sys <- parseFileSpec f
       run' flags sys

run flags Nothing = do
        input <- getContents
        sys <- parseSpec input
        run' flags sys

run' :: [Flag]
     -> Either (ParseError Char Dec) (System Int)
     -> IO ()

run' flags sys =
    case sys of
      Left err   -> printError err
      Right sys' -> do
          let ws = warnings sys'
          reportSystemWarnings ws
          case errors (useForce flags) sys' of
              Left err'  -> reportSystemError err'
              Right sysT -> do
                  when (exitWerror flags ws) $ exitWith (ExitFailure 1)
                  if toPaganini flags then writeSpec sys' (output flags)
                                      else if Tune `elem` flags then runTuner sys' flags
                                                                else if Generate `elem` flags then runSampler sys' flags
                                                                                              else runCompiler sys' sysT flags

exitWerror :: [Flag] -> WarningMonad () -> Bool
exitWerror flags ws = isLeft ws && Werror `elem` flags

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

outputSpec :: Maybe FilePath -> PSystem Double -> IO ()
outputSpec Nothing sys'  = B.putStr $ encode (toSystemT $ system sys')
outputSpec (Just f) sys' = B.writeFile f $ encode (toSystemT $ system sys')

runTuner :: System Int -> [Flag] -> IO ()
runTuner sys flags =
    case fromPaganini flags of
        Nothing -> do
            let arg = T.defaultArgs sys
            pag <- T.runPaganini sys T.Regular (Just $ arg { T.precision = getPrecision sys
                                                           , T.maxiters  = fromMaybe (T.maxiters arg)
                                                                                     (getMaxIter sys) })
            case pag of
                Left err -> printError err
                Right sys' -> outputSpec (output flags) sys'
        Just s  -> do
            pag <- T.readPaganini sys T.Regular s
            case pag of
                Left err   -> printError err
                Right sys' -> outputSpec (output flags) sys'

runCompiler :: System Int -> SystemType -> [Flag] -> IO ()
runCompiler sys sysT flags =
    case fromPaganini flags of
      Nothing -> do
          let arg = T.defaultArgs sys
          pag <- T.runPaganini sys T.Cummulative (Just $ arg { T.precision = getPrecision sys
                                                             , T.maxiters  = fromMaybe (T.maxiters arg)
                                                                                       (getMaxIter sys) })
          case pag of
            Left err   -> printError err
            Right sys' -> confCompiler sys' (output flags) sysT
      Just s  -> do
          pag <- T.readPaganini sys T.Cummulative s
          case pag of
            Left err   -> printError err
            Right sys' -> confCompiler sys' (output flags) sysT

invalidGenType :: String -> IO ()
invalidGenType str = do
    hPutStr stderr $ "[Error] Type \'" ++ str ++ "\' does not name a valid type."
    exitWith (ExitFailure 1)

invalidBounds :: Int -> Int -> IO ()
invalidBounds lb ub = do
    hPutStr stderr $ "[Error] Lower bound " ++ show lb ++ " is greater"
        ++ " than the upper bound " ++ show ub ++ "."
    exitWith (ExitFailure 1)

runSampler :: System Int -> [Flag] -> IO ()
runSampler sys flags = do
    let lb  = getStrLowerBound sys
    let ub  = getStrUpperBound sys
    let str = getGenType sys

    -- Check if the given type is valid
    when (str `S.notMember` types sys) $ invalidGenType str

    -- Check if the given lower and upper bounds is sensible
    when (lb > ub) $ invalidBounds lb ub

    case fromPaganini flags of
        Nothing -> do
            let arg = T.defaultArgs sys
            pag <- T.runPaganini sys T.Cummulative (Just $ arg { T.precision = getPrecision sys
                                                               , T.maxiters  = fromMaybe (T.maxiters arg)
                                                                                         (getMaxIter sys) })
            case pag of
                Left err -> printError err
                Right sys' -> do
                    sample <- sampleStrIO sys' str lb ub
                    B.putStrLn $ encode sample
                    exitSuccess
        Just s  -> do
            pag <- T.readPaganini sys T.Cummulative s
            case pag of
                Left err   -> printError err
                Right sys' -> do
                    sample <- sampleStrIO sys' str lb ub
                    B.putStrLn $ encode sample
                    exitSuccess

reportSystemError :: SystemError -> IO ()
reportSystemError err = do
    hPrint stderr err
    exitWith (ExitFailure 1)

reportSystemWarnings :: WarningMonad () -> IO ()
reportSystemWarnings (Left ws) = hPrint stderr ws
reportSystemWarnings _         = return ()

main :: IO ()
main = do
    (ops, fs) <- getArgs >>= parse
    case fs of
      []     -> if Stdin `elem` ops then run ops Nothing
                else do hPutStr stderr (usageInfo usageHeader options)
                        exitSuccess
      (f:_)  -> do run ops (Just f)
                   exitSuccess
