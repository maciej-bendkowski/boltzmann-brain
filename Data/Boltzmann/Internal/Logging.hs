{-|
 Module      : Data.Boltzmann.Internal.Logging
 Description : Basic logging utilities.
 Copyright   : (c) Maciej Bendkowski, 2017-2021

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
    , hint
    , hint'
    , fail
    , fail'
    ) where

import Prelude hiding (log, fail)

import System.IO
import System.Exit

import System.Console.Pretty

import Data.Boltzmann.Internal.Utils

data Level = Info
           | Warning
           | Hint
           | Error

instance Show Level where
    show Info    = "INF"
    show Warning = "WAR"
    show Hint    = "TIP"
    show Error   = "ERR"

lvlColor :: Level -> Color
lvlColor Info    = Blue
lvlColor Warning = Yellow
lvlColor Hint    = Green
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

-- | Logs a HINT message.
hint :: String -> IO ()
hint s = report Log { lvl = Hint , msg = s }

-- | Logs a HINT message and terminates.
hint' :: String -> IO a
hint' s = do
    report Log { lvl = Hint , msg = s }
    exitWith (ExitFailure 1)

-- | Logs a WARNING message and terminates.
warn' :: String -> IO a
warn' s = do
    report Log { lvl = Warning, msg = s }
    exitWith (ExitFailure 1)

-- | Logs an ERROR message.
fail :: String -> IO ()
fail s = report Log { lvl = Error, msg = s }

-- | Logs an ERROR message and terminates.
fail' :: String -> IO a
fail' s = do
    report Log { lvl = Error, msg = s }
    exitWith (ExitFailure 1)
