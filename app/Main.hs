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

import Data.List (nub)
import Numeric.LinearAlgebra hiding (size)

import Numeric

import Data.Boltzmann.System
import Data.Boltzmann.System.Oracle
import Data.Boltzmann.System.Parser
import Data.Boltzmann.System.Errors

import qualified Data.Boltzmann.System.Paganini as P
import qualified Data.Boltzmann.Compiler.Haskell.Algebraic as A
import qualified Data.Boltzmann.Compiler.Haskell.Rational as R

data Flag = SingEpsilon String
          | SysEpsilon String
          | Singularity String
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
options = [Option "e" ["eps"] (ReqArg SingEpsilon "e")
            "Singularity approximation precision. Defaults to 1.0e-9.",

          Option "p" ["precision"] (ReqArg SysEpsilon "p")
            "Evaluation approximation precision. Defaults to 1.0e-9.",

          Option "g" ["to-paganini"] (NoArg OutputPaganini)
            "Output a suitable Paganini specification for the given system.",

          Option "t" ["from-paganini"] (ReqArg InputPaganini "t")
            "Input a suitable Paganini tuning vector for the given system.",

           Option "r" ["rho"] (ReqArg Singularity "r")
            "Optional singularity parameter used to evaluate the system.",

           Option "m" ["module"] (ReqArg ModuleName "m")
            "The resulting Haskell module name. Defaults to Sampler.",

           Option "i" ["with-io"] (NoArg WithIO)
            "Whether to generate IO actions for the samplers.",

           Option "l" ["with-lists"] (NoArg WithLists)
            "Whether to generate lists samplers for all types.",

           Option "d" ["with-show"] (NoArg WithShow)
            "Whether to generate data types deriving Show.",

           Option "f" ["force"] (NoArg Force)
            "Whether to skips the well-foundness check.",

           Option "v" ["version"] (NoArg Version)
            "Prints the program version number.",

           Option "h?" ["help"] (NoArg Help)
            "Prints this help message."]

usageHeader :: String
usageHeader = "Usage: bb [OPTIONS...]"

versionHeader :: String
versionHeader = "Boltzmann brain v1.1 (c) Maciej Bendkowski 2017"

compilerTimestamp :: String
compilerTimestamp = "Boltzmann brain v1.1"

parseFloating :: String -> Rational
parseFloating s = (fst $ head (readFloat s)) :: Rational

getSingEpsilon :: [Flag] -> Double
getSingEpsilon (SingEpsilon eps : _) = fromRational $ parseFloating eps
getSingEpsilon (_:fs)                = getSingEpsilon fs
getSingEpsilon []                    = fromRational 1.0e-9

getSysEpsilon :: [Flag] -> Double
getSysEpsilon (SysEpsilon eps : _) = fromRational $ parseFloating eps
getSysEpsilon (_:fs)               = getSysEpsilon fs
getSysEpsilon []                   = fromRational 1.0e-9

getSingularity :: [Flag] -> Maybe Double
getSingularity (Singularity s : _) = Just (fromRational $ parseFloating s)
getSingularity (_:fs)              = getSingularity fs
getSingularity []                  = Nothing

getModuleName :: [Flag] -> String
getModuleName (ModuleName name : _) = name
getModuleName (_:fs)                = getModuleName fs
getModuleName []                    = "Sampler"

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
                      Left err' -> reportSystemError err'
                      Right sysT   -> if toPaganini flags then P.paganiniSpecification sys'
                                                          else runCompiler sys' sysT flags

oracle :: [Flag] -> System Int -> PSystem Double
oracle flags sys = case getSingularity flags of
                           Nothing -> let singEps = getSingEpsilon flags
                                          sysEps  = getSysEpsilon flags
                                          rho     = singularity sys singEps
                                       in
                                          parametrise sys rho sysEps

                           Just rho -> let sysEps = getSysEpsilon flags in
                                         parametrise sys rho sysEps

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
          let sys' = oracle flags sys
          confCompiler sys' sysT flags
      Just s  -> do
          let spec = P.toPSpec sys
          pag <- parsePaganini spec s
          case pag of
            Left err     -> printError err
            Right (rho,us, ts) -> do
                let ts'  = fromList ts
                let sys' = P.parametrise sys rho ts' us
                confCompiler sys' sysT flags

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
