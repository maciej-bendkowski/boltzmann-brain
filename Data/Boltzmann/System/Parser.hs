{-|
 Module      : Data.Boltzmann.System.Parser
 Description : General system parsing utilities.
 Copyright   : (c) Maciej Bendkowski, 2017-2020

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.System.Parser
  ( parseSpec
  )
where

import           Text.Megaparsec
import           Data.Void

import qualified Data.Boltzmann.System         as S

import qualified Data.Boltzmann.System.Parser.Algebraic
                                               as A

-- | Parses the given system specification.
parseSpec :: String -> IO (Either (ParseErrorBundle String Void) (S.System Int))
parseSpec s = return $ parse A.systemStmt "" s
