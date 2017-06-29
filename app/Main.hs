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

import Numeric

import Data.Boltzmann.System
import Data.Boltzmann.System.Oracle
import Data.Boltzmann.System.Parser
import Data.Boltzmann.System.Errors

import Data.Boltzmann.Compiler.Haskell.MaybeT

data Flag = SingEpsilon String
          | SysEpsilon String
          | Singularity String
          | ModuleName String
          | WithLists
          | WithIO
          | Version
          | Help
            deriving (Eq)

options :: [OptDescr Flag]
options = [Option "e" ["eps"] (ReqArg SingEpsilon "e")
            "Singularity approximation precision. Defaults to 1.0e-6.",

          Option "p" ["precision"] (ReqArg SysEpsilon "p")
            "Evaluation approximation precision. Defaults to 1.0e-6.",

           Option "r" ["rho"] (ReqArg Singularity "r")
            "Optional singularity parameter used to evaluate the system.",

           Option "m" ["module"] (ReqArg ModuleName "m")
            "The resulting Haskell module name. Defaults to Sampler.",

           Option "i" ["with-io"] (NoArg WithIO)
            "Whether to generate IO actions for the samplers.",

           Option "l" ["with-lists"] (NoArg WithLists)
            "Whether to generate lists samplers for all types.",

           Option "v" ["version"] (NoArg Version)
            "Prints the program version number.",

           Option "h?" ["help"] (NoArg Help)
            "Prints this help message."]

usageHeader :: String
usageHeader = "Usage: bb [OPTIONS...]"

versionHeader :: String
versionHeader = "Boltzmann brain v1.0 (c) Maciej Bendkowski 2017"

compilerTimestamp :: String
compilerTimestamp = "Boltzmann brain v1.0"

parseFloating :: String -> Rational
parseFloating s = (fst $ head (readFloat s)) :: Rational

getSingEpsilon :: [Flag] -> Double
getSingEpsilon (SingEpsilon eps : _) = fromRational $ parseFloating eps
getSingEpsilon (_:fs) = getSingEpsilon fs
getSingEpsilon [] = fromRational 1.0e-6

getSysEpsilon :: [Flag] -> Double
getSysEpsilon (SysEpsilon eps : _) = fromRational $ parseFloating eps
getSysEpsilon (_:fs) = getSysEpsilon fs
getSysEpsilon [] = fromRational 1.0e-6

getSingularity :: [Flag] -> Maybe Double
getSingularity (Singularity s : _) = Just (fromRational $ parseFloating s)
getSingularity (_:fs) = getSingularity fs
getSingularity [] = Nothing

getModuleName :: [Flag] -> String
getModuleName (ModuleName name : _) = name
getModuleName (_:fs) = getModuleName fs
getModuleName [] = "Sampler"

useIO :: [Flag] -> Bool
useIO flags = WithIO `elem` flags

genLists :: [Flag] -> Bool
genLists flags = WithLists `elem` flags

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
      Right sys' -> runCompiler sys' flags

oracle :: [Flag] -> System Int -> PSystem Double
oracle flags sys = case getSingularity flags of
                           Nothing -> let singEps = getSingEpsilon flags
                                          sysEps = getSysEpsilon flags
                                          rho = singularity sys singEps
                                       in
                                          parametrise sys rho sysEps

                           Just rho -> let sysEps = getSysEpsilon flags in
                                         parametrise sys rho sysEps

confCompiler :: PSystem Double -> [Flag] -> IO ()
confCompiler sys flags = do
    let conf = Conf { paramSys    = sys
                    , moduleName  = getModuleName flags
                    , compileNote = compilerTimestamp
                    , withIO      = useIO flags
                    , withLists   = genLists flags
                    }
    compile conf


runCompiler :: System Int -> [Flag] -> IO ()
runCompiler sys flags = case errors sys of
    Left err -> reportSystemError err
    Right _ -> do
        let sys' = oracle flags sys
        confCompiler sys' flags

reportSystemError :: SystemError -> IO ()
reportSystemError err = do
    hPrint stderr err
    exitWith (ExitFailure 1)

main :: IO ()
main = do
    (ops, fs) <- getArgs >>= parse
    mapM_ (run ops) fs
    exitSuccess
