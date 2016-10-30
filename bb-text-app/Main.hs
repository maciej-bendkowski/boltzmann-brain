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

import Compiler.Text.Boltzmann

import Jacobian

import Oracle
import qualified Oracle.Newton as Newton
import qualified Oracle.Banach as Banach

currentTime :: IO ()
currentTime = print =<< getZonedTime

data Flag = SingEpsilon String
          | SysEpsilon String
          | Singularity String
          | Oracle String
          | Version
          | Help
            deriving (Eq)

data BoltzmannOracle = Newton
                     | Banach

options :: [OptDescr Flag]
options = [Option "p" ["precision"] (ReqArg SingEpsilon "p")
            "Singularity approximation precision. Defaults to 1.0e-6.",

          Option "e" ["eps"] (ReqArg SysEpsilon "e")
            "Evaluation approximation precision. Defaults to 1.0e-6.",

           Option "s" ["sing"] (ReqArg Singularity "s")
            "Optional singularity parameter used to evaluate the system.",

           Option "o" ["oracle"] (ReqArg Oracle "o")
            "Boltzmann oracle (newton|banach). Defauts to newton.",

           Option "v" ["version"] (NoArg Version)
            "Prints the program version number.",

           Option "h?" ["help"] (NoArg Help)
            "Prints this help message."]

usageHeader :: String
usageHeader = "Usage: bb-text [OPTIONS...]"

versionHeader :: String
versionHeader = "boltzmann-brain (text compiler) ALPHA version (c) Maciej Bendkowski 2016"

parseFloating :: String -> Rational
parseFloating s = (fst $ head (readFloat s)) :: Rational

getSingEpsilon :: [Flag] -> P 
getSingEpsilon (SingEpsilon eps : _) = fromRational $ parseFloating eps
getSingEpsilon (_:fs) = getSingEpsilon fs
getSingEpsilon [] = fromRational 1.0e-6

getSysEpsilon :: [Flag] -> P
getSysEpsilon (SysEpsilon eps : _) = fromRational $ parseFloating eps
getSysEpsilon (_:fs) = getSysEpsilon fs
getSysEpsilon [] = fromRational 1.0e-6

getSingularity :: [Flag] -> Maybe P
getSingularity (Singularity s : _) = Just (fromRational $ parseFloating s)
getSingularity (_:fs) = getSingularity fs
getSingularity [] = Nothing

getOracle :: [Flag] -> BoltzmannOracle
getOracle (Oracle name : fs)
    | name == "banach" = Banach
    | name == "newton" = Newton
    | otherwise = getOracle fs
getOracle (_:fs) = getOracle fs
getOracle [] = Newton 

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
      Right sys -> runCompiler sys flags

type P = BigFloat (PrecPlus20 Eps1)
type NewtonSystem = BoltzmannSystem (Newton.State P) P
type BanachSystem = BoltzmannSystem (Banach.State P) P

getBoltzmannNewton :: [Flag] -> System Integer -> NewtonSystem
getBoltzmannNewton flags sys = case getSingularity flags of
                           Nothing -> let singEps = getSingEpsilon flags
                                          sysEps = getSysEpsilon flags in
                                          toBoltzmann sys singEps sysEps
                           
                           Just s -> let sysEps = getSysEpsilon flags in
                                         toBoltzmannS sys s sysEps

getBoltzmannBanach :: [Flag] -> System Integer -> BanachSystem
getBoltzmannBanach flags sys = case getSingularity flags of
                           Nothing -> let singEps = getSingEpsilon flags
                                          sysEps = getSysEpsilon flags in
                                          toBoltzmann sys singEps sysEps
                           
                           Just s -> let sysEps = getSysEpsilon flags in
                                         toBoltzmannS sys s sysEps
  
confCompiler sys flags = do
    time <- getZonedTime
    let conf = Configuration { paramSys = sys }
    compile conf

runCompiler :: System Integer -> [Flag] -> IO ()
runCompiler sys flags = case errors sys of
    Left err -> reportSystemError err
    Right _ -> case getOracle flags of
                 Newton -> let sys' = getBoltzmannNewton flags sys in
                               confCompiler sys' flags 
                 Banach -> let sys' = getBoltzmannBanach flags sys in
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
