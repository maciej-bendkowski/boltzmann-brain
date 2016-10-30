-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Main (main) where

import System.IO
import System.Exit
import System.Console.GetOpt
import System.Environment
    
import Data.List (nub)

import Data.Number.Fixed
import Data.Number.BigFloat

import Data.Time
import Data.Maybe

import Numeric

import System
import System.Boltzmann

import Errors
import Parser
import Compiler

import Jacobian

import Oracle
import Oracle.Newton

currentTime :: IO ()
currentTime = print =<< getZonedTime

data Flag = SingEpsilon String
          | SysEpsilon String
          | Singularity String
          | ModuleName String
          | Version
          | Help
            deriving (Eq)

options :: [OptDescr Flag]
options = [Option "p" ["precision"] (ReqArg SingEpsilon "p")
            "Singularity approximation precision. Defaults to 1.0e-6.",

          Option "e" ["eps"] (ReqArg SysEpsilon "e")
            "Evaluation approximation precision. Defaults to 1.0e-6.",

           Option "s" ["sing"] (ReqArg Singularity "s")
            "Optional singularity parameter used to evaluate the system.",

           Option "m" ["module"] (ReqArg ModuleName "m")
            "The resulting Haskell module name. Defaults to Main.",

           Option "v" ["version"] (NoArg Version)
            "Prints the program version number.",

           Option "h?" ["help"] (NoArg Help)
            "Prints this help message."]

usageHeader :: String
usageHeader = "Usage: bb [OPTIONS...]"

versionHeader :: String
versionHeader = "boltzmann-brain ALPHA version (c) Maciej Bendkowski 2016"

compilerTimestamp :: String -> String 
compilerTimestamp time = "boltzmann-brain ALPHA (" ++ time ++ ")"

parseFloating :: String -> Rational
parseFloating s = (fst $ head (readFloat s)) :: Rational

getSingEpsilon :: [Flag] -> BigFloat (PrecPlus20 Eps1)
getSingEpsilon (SingEpsilon eps : _) = fromRational $ parseFloating eps
getSingEpsilon (_:fs) = getSingEpsilon fs
getSingEpsilon [] = fromRational 1.0e-6

getSysEpsilon :: [Flag] -> BigFloat (PrecPlus20 Eps1)
getSysEpsilon (SysEpsilon eps : _) = fromRational $ parseFloating eps
getSysEpsilon (_:fs) = getSysEpsilon fs
getSysEpsilon [] = fromRational 1.0e-6

getSingularity :: [Flag] -> Maybe (BigFloat (PrecPlus20 Eps1))
getSingularity (Singularity s : _) = Just (fromRational $ parseFloating s)
getSingularity (_:fs) = getSingularity fs
getSingularity [] = Nothing

getModuleName :: [Flag] -> String
getModuleName (ModuleName name : _) = name
getModuleName (_:fs) = getModuleName fs
getModuleName [] = "Main"

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
      Left err -> printError err
      Right sys -> runCompiler singEps sysEps sing module' sys
    where
        module' = getModuleName flags
        singEps = getSingEpsilon flags
        sysEps = getSysEpsilon flags
        sing = getSingularity flags

runCompiler' :: (Show b, Real b) => BoltzmannSystem (State b) b -> String -> IO ()
runCompiler' sys module' = do
    time <- getZonedTime
    compile sys module' (compilerTimestamp $ show time)

runCompiler singEps sysEps sing module' sys = case errors sys of
    Left err -> reportSystemError err
    Right _ -> case sing of
                 Nothing -> runCompiler' (toBoltzmann sys singEps sysEps) module'
                 Just s -> runCompiler' (toBoltzmannS sys s sysEps) module'

reportSystemError :: SystemError -> IO ()
reportSystemError err = do 
    hPrint stderr err
    exitWith (ExitFailure 1)

main :: IO ()
main = do
    (ops, fs) <- getArgs >>= parse
    mapM_ (run ops) fs
    exitSuccess
