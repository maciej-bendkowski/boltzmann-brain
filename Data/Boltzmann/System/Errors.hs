{-|
 Module      : Data.Boltzmann.System.Errors
 Description : Various error handling utilities.
 Copyright   : (c) Maciej Bendkowski, 2017-2018

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 Common error utilities for combinatorial systems.
 -}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Boltzmann.System.Errors
    ( errors
    ) where

import Prelude hiding (log)

import Control.Monad (unless)

import Data.Map (Map)
import qualified Data.Map.Strict as M

import qualified Data.Set as S

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

import Data.Maybe (mapMaybe)

import Data.Char (isUpper)
import Text.Read (readMaybe)

import System.Exit

import Data.Boltzmann.System
import Data.Boltzmann.System.Utils

import Data.Boltzmann.Internal.Utils
import qualified Data.Boltzmann.Internal.Logging as L

-- | Finds the closest (in terms of editing distance)
--   type in the given system.
closestType :: System a -> String -> String
closestType sys = closest types'
    where types' = S.toList $ types sys

-- | Semantic system errors referring to invalid
--   input data, for instance ill-founded systems.
class SystemErr a where

    -- | System error message.
    report :: a -> String

    -- | System error hint message.
    hint :: a -> String

-- | Existential error type.
data ErrorExt = forall a. (SystemErr a) => ErrorExt a

-- | Constructor arguments reference non-existing types.
data ArgRefError a =
        ArgRefError { argRefType :: String   -- ^ Type name.
                    , argRefCons :: String   -- ^ Constructor name.
                    , argRefArg  :: String   -- ^ Argument name.
                    , argRefSys  :: System a -- ^ Corresponding system.
                    }

argRefErrors :: System a -> [ErrorExt]
argRefErrors sys = concatMap argRefErrorType (M.toList $ defs sys)
    where
        types' = types sys
        argRefErrorType (typ, consL) =
            concatMap (argRefErrorCons typ) consL

        argRefErrorCons typ cons =
            mapMaybe (argRefErrorArg typ cons) (args cons)

        argRefErrorArg typ cons arg
            | argName arg `S.member` types' = Nothing
            | otherwise = Just $ ErrorExt ArgRefError { argRefType = typ
                                                      , argRefArg  = argName arg
                                                      , argRefCons = func cons
                                                      , argRefSys  = sys
                                                      }
instance SystemErr (ArgRefError a) where
    report err = "Invalid argument type "
            ++ arg' ++ " in constructor "
            ++ cons' ++ " of type "
            ++ typ' ++ "."
        where arg'  = quote $ argRefArg err
              cons' = quote $ argRefCons err
              typ'  = argRefType err

    hint err = "Perhaps you meant " ++ type' ++ " instead?"
        where (system', arg') = (argRefSys err, argRefArg err)
              type'           = quote $ closestType system' arg'

-- | Constructor name clashes with an existing type.
data ConsRefError =
        ConsRefError { consRefType :: String -- ^ Type name.
                     , consRefCons :: String -- ^ Constructor name.
                     }

consRefErrors :: System a -> [ErrorExt]
consRefErrors sys =
        concatMap consRefErrorsType (M.toList $ defs sys)
    where
        types' = types sys
        consRefErrorsType (typ, consL) =
            mapMaybe (consRefErrorsCons typ) consL

        consRefErrorsCons typ cons
            | null (args cons) && func cons `S.member` types' =
                    Just $ ErrorExt ConsRefError { consRefType = typ
                                                 , consRefCons = func cons
                                                 }
            | otherwise = Nothing

instance SystemErr ConsRefError where
    report err = "Invalid constructor "
            ++ cons' ++ " of type "
            ++ typ' ++ ". " ++ cons'
            ++ " names a declared type."
        where cons' = quote $ consRefCons err
              typ'  = consRefType err

    hint _ = "Use a different constructor name instead."

-- | Clashing constructor names.
newtype ClashConsError =
        ClashConsError { clashConTypes :: [String] -- ^ Type names.
                       }

duplicates :: System a -> [String]
duplicates sys = map fst $ filter gather $ MultiSet.toOccurList ms
    where gather (_,n) = n /= 1
          ms           = consNames sys

consNames :: System a -> MultiSet String
consNames sys = MultiSet.unions (map insT $ M.elems (defs sys))
    where insT = MultiSet.fromList . map func

clashConsErrors :: System a -> [ErrorExt]
clashConsErrors sys =
    let dups' = duplicates sys
    in if null dups' then []
                     else [ErrorExt
                            ClashConsError { clashConTypes = dups' }]

instance SystemErr ClashConsError where
    report err = "Clasing constructor names: "
            ++ csv (map quote $ clashConTypes err) ++ "."

    hint _ = "Declared constructor names have to be unique."

-- | System defines no finite structures.
data InfLangError = InfLangError

infLangErrors :: System a -> [ErrorExt]
infLangErrors sys =
        [ErrorExt InfLangError | not (hasAtoms sys) && null (seqTypes sys)]

instance SystemErr InfLangError where
    report _ = "Given system defines no finite structures."
    hint _ = "Declare nullary type constructors or use list constructions."

-- | System is not well-founded at zero.
data WellFoundedError = WellFoundedError

wellFoundedErrors :: System Int -> [ErrorExt]
wellFoundedErrors sys =
    [ErrorExt WellFoundedError |
        not (isEmptyAtZero sys) || not (wellFoundedAtZero sys)]

instance SystemErr WellFoundedError where
    report _ = "Given system is not well-founded at zero."
    hint _ = "Examine the system specification or use the --force flag."

-- | Invalid frequencies.
newtype FreqError =
        FreqError { freqTypes :: [String] -- ^ Type names.
                  }

incorrectFrequencies :: System a -> [String]
incorrectFrequencies sys = concatMap incF $ M.elems (defs sys)
    where incF cons  = map func $ filter incF' cons
          incF' cons = case frequency cons of
                         Nothing -> False
                         Just f  -> 0.0 > f || 1.0 < f

freqErrors :: System a -> [ErrorExt]
freqErrors sys = [ErrorExt FreqError { freqTypes = freqs } | not (null freqs)]
    where freqs = incorrectFrequencies sys

instance SystemErr FreqError where
    report err = "Incorrect frequencies corresponding to constructors: "
            ++ csv (map quote $ freqTypes err) ++ "."

    hint _ = "Declared frequencies have to be reals in the interval (0.0, 1.0)."

-- | Unsupported system type.
newtype SysTypeError =
        SysTypeError { sysTypeMsg :: String -- ^ System type message.
                   }

-- | Checks if the specification type is supported.
supportedSystemType :: System a -> Either SysTypeError SystemType
supportedSystemType sys =
    case systemType sys of
      (Unsupported s) -> Left SysTypeError { sysTypeMsg = s }
      sysT            -> Right sysT

instance SystemErr SysTypeError where
    report = sysTypeMsg
    hint _ = "Supported systems include algebraic and"
            ++ " strongly-connected, interruptible rational specifications."

-- | General error including various annotation errors.
data GenErr = GenErr { errorMsg :: String -- ^ General error message.
                     , hintMsg  :: String -- ^ General hint message.
                     }

instance SystemErr GenErr where
    report = errorMsg
    hint = hintMsg

-- | Reports the given error with its hint and terminates.
reportError :: ErrorExt -> IO ()
reportError (ErrorExt err) = do
    L.fail (report err)
    L.hint (hint err)

checkSysType :: SystemErr a => Either a b -> IO b
checkSysType (Right x)  = return x
checkSysType (Left err) = do
    L.fail (report err)
    L.hint' (hint err)

checkErrors :: [ErrorExt] -> IO ()
checkErrors errs = do
    mapM_ reportError errs
    unless (null errs)
        (exitWith $ ExitFailure 1)

-- | Some trivial errors.
trivialErrors :: Format -> System a -> [ErrorExt]
trivialErrors format sys
    = concat [argRefErrors sys
              ,consRefErrors sys
              ,freqErrors sys
              ]
        ++ -- note: the following errors are inherent to algebraic specification formats.
            concat [clashConsErrors sys | isAlgebraicF format]

-- | Some less trivial errors.
otherErrors :: System Int -> [ErrorExt]
otherErrors sys
    = infLangErrors sys
        ++ wellFoundedErrors sys

-- | Checks whether the given input system is correct, yielding its type.
--   Otherwise, terminate with an appropriate system error message.
errors :: Format -> Bool -> System Int -> IO SystemType
errors format force sys =
    if force then checkSysType $ supportedSystemType sys -- the force is strong with this one.
             else do
                checkErrors (annotationErrors sys $ annotations sys)
                checkErrors (trivialErrors format sys)
                checkErrors (otherErrors sys)
                errors format True sys

annotationErrors :: System a -> Map String String -> [ErrorExt]
annotationErrors sys ann
    = mapMaybe ($ ann)
        [precisionAnnotation
        ,maxIterAnnotation
        ,moduleAnnotation
        ,samplesAnnotation
        ,lowerBoundAnnotation
        ,upperBoundAnnotation
        ,incoherentBoundsAnnotation
        ,incorrectGenerateAnnotation sys
        ]

incorrectIntError :: String -> Maybe ErrorExt
incorrectIntError typ = Just
    (ErrorExt GenErr { errorMsg = "Incorrect " ++ typ ++ " annotation."
                     , hintMsg  = "Use a positive integer value."
                     })

samplesAnnotation :: Map String String -> Maybe ErrorExt
samplesAnnotation ann =
    case "samples" `M.lookup` ann of
      Nothing -> Nothing
      Just x -> case readMaybe x :: Maybe Int of
                  Nothing -> incorrectIntError "@samples"
                  Just x' -> if x' > 0 then Nothing
                                       else incorrectIntError "@samples"

lowerBoundAnnotation :: Map String String -> Maybe ErrorExt
lowerBoundAnnotation ann =
    case "lowerBound" `M.lookup` ann of
      Nothing -> Nothing
      Just x -> case readMaybe x :: Maybe Int of
                  Nothing -> incorrectIntError "@lowerBound"
                  Just x' -> if x' > 0 then Nothing
                                       else incorrectIntError "@lowerBound"

upperBoundAnnotation :: Map String String -> Maybe ErrorExt
upperBoundAnnotation ann =
    case "upperBound" `M.lookup` ann of
      Nothing -> Nothing
      Just x -> case readMaybe x :: Maybe Int of
                  Nothing -> incorrectIntError "@upperBound"
                  Just x' -> if x' > 0 then Nothing
                                       else incorrectIntError "@upperBound"

incoherentBoundsErrors :: Maybe ErrorExt
incoherentBoundsErrors = Just
    (ErrorExt GenErr { errorMsg = "Incorrect @lowerBound and @upperBound annotations."
                     , hintMsg  = "The lower bound must be less or equal to the upper bound."
                     })

incoherentBoundsAnnotation :: Map String String -> Maybe ErrorExt
incoherentBoundsAnnotation ann = do
    lbs <- "lowerBound" `M.lookup` ann
    lb  <- readMaybe lbs :: Maybe Int

    ubs <- "upperBound" `M.lookup` ann
    ub  <- readMaybe ubs :: Maybe Int
    if ub < lb then incoherentBoundsErrors
               else Nothing

incorrectGenerateError :: System a -> String -> Maybe ErrorExt
incorrectGenerateError sys typ = Just
    (ErrorExt GenErr { errorMsg = "Incorrect @generate annotation."
                     , hintMsg  = quote typ ++ " does not name a declared type."
                            ++ " Perhaps you meant " ++ quote typ' ++ " instead?"
                     })
    where
        typ' = closestType sys typ

incorrectGenerateAnnotation :: System a -> Map String String -> Maybe ErrorExt
incorrectGenerateAnnotation sys ann = do
    typ <- "generate" `M.lookup` ann
    if typ `S.member` types sys then Nothing
                                else incorrectGenerateError sys typ

precisionError :: Maybe ErrorExt
precisionError = Just
    (ErrorExt GenErr { errorMsg = "Incorrect @precision annotation."
                     , hintMsg  = "Use a positive double precision value."
                     })

precisionAnnotation :: Map String String -> Maybe ErrorExt
precisionAnnotation ann =
    case "precision" `M.lookup` ann of
      Nothing -> Nothing
      Just x -> case readMaybe x :: Maybe Double of
                  Nothing -> precisionError
                  Just x' -> if x' > 0 then Nothing
                                       else precisionError

maxIterError :: Maybe ErrorExt
maxIterError = Just
    (ErrorExt GenErr { errorMsg = "Incorrect @maxiter annotation."
                     , hintMsg  = "Use a positive integer value."
                     })

maxIterAnnotation :: Map String String -> Maybe ErrorExt
maxIterAnnotation ann =
    case "maxiter" `M.lookup` ann of
      Nothing -> Nothing
      Just x -> case readMaybe x :: Maybe Int of
                  Nothing -> maxIterError
                  Just x' -> if x' > 0 then Nothing
                                       else maxIterError

moduleError :: Maybe ErrorExt
moduleError = Just
    (ErrorExt GenErr { errorMsg = "Incorrect @module annotation."
                     , hintMsg  = "Module names have to start with an uppercase letter."
                     })

moduleAnnotation :: Map String String -> Maybe ErrorExt
moduleAnnotation ann =
    case "module" `M.lookup` ann of
      Nothing -> Nothing
      Just x -> if isUpper (head x) then Nothing
                                    else moduleError
