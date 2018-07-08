{-|
 Module      : Data.Boltzmann.Internal.Logging
 Description : Basic logging utilities.
 Copyright   : (c) Maciej Bendkowski, 2017-2018

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 General logging utilities.
 -}
module Data.Boltzmann.Internal.Logging
    ( Log(..)
    , info
    , warn
    , warn'
    , fail
    ) where

import Prelude hiding (log, fail)

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format

import System.IO
import System.Exit

import System.Console.Pretty

getTime :: IO String
getTime = do
    now <- getCurrentTime
    timeZone <- getCurrentTimeZone
    let t = utcToLocalTime timeZone now
    return $ formatTime defaultTimeLocale "%d-%m-%Y %H:%M:%S" t

data Level = Info
           | Warning
           | Error

instance Show Level where
    show Info    = "INF"
    show Warning = "WAR"
    show Error   = "ERR"

lvlColor :: Level -> Color
lvlColor Info    = Blue
lvlColor Warning = Yellow
lvlColor Error   = Red

brackets :: IO a -> IO ()
brackets m = hPutStr stderr "[" >> m >> hPutStr stderr "] " -- note the trailing space

parens :: IO a -> IO ()
parens m = hPutStr stderr "(" >> m >> hPutStr stderr ") " -- note the trailing space

data Log = Log { lvl :: Level   -- ^ Logging level.
               , msg :: String  -- ^ Logging message.
               }

printLabel :: Log -> IO ()
printLabel log = do
    inColor <- supportsPretty
    let label  = show (lvl log)
    let format = style Bold . color (lvlColor $ lvl log)
    let x = if inColor then format label
                       else label
    hPutStr stderr x

printTime :: String -> IO ()
printTime time = do
    inColor <- supportsPretty
    let format = style Italic
    let x = if inColor then format time
                       else time
    hPutStr stderr x

-- | Reports a logging message.
report :: Log -> IO ()
report log = do
    time <- getTime
    brackets $ printLabel log
    parens $ printTime time
    hPutStrLn stderr $ msg log

-- | Logs an INFO message.
info :: String -> IO ()
info s = report Log { lvl = Info, msg = s }

-- | Logs a WARNING message.
warn :: String -> IO ()
warn s = report Log { lvl = Warning, msg = s }

-- | Logs a WARNING message and terminates.
warn' :: String -> IO a
warn' s = do
    report Log { lvl = Warning, msg = s }
    exitWith (ExitFailure 1)

-- | Logs an ERROR message and terminates.
fail :: String -> IO a
fail s = do
    report Log { lvl = Error, msg = s }
    exitWith (ExitFailure 1)
