{-|
 Module      : Data.Boltzmann.System.Parser
 Description : General system parsing utilities.
 Copyright   : (c) Maciej Bendkowski, 2017-2019

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.System.Parser
    ( parseSpec
    ) where

import Text.Megaparsec
import Data.Void

import qualified Data.Boltzmann.System as S

import qualified Data.Boltzmann.System.Parser.Algebraic as A
import qualified Data.Boltzmann.System.Parser.Rational as R

-- | Parses the given system specification.
parseSpec :: S.Format -> String
          -> IO (Either (ParseErrorBundle String Void) (S.System Int))

parseSpec S.RationalF s  = return $ parse R.systemStmt "" s
parseSpec S.AlgebraicF s = return $ parse A.systemStmt "" s
