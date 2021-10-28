
module Data.Boltzmann.System.Paganini
  ( toPaganini
  )
where

import           Data.Boltzmann.System   hiding ( value )

import           Data.Paganini

import           Control.Monad                  ( replicateM )

import           Prelude                 hiding ( seq )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )

mkVariables :: System a -> Spec (Map String Let)
mkVariables sys = do
  xs <- replicateM (size sys) variable
  return (M.fromList $ zip (M.keys $ defs sys) xs)

mkSeqVariables :: System a -> Map String Let -> Spec (Map String Def)
mkSeqVariables sys variables = do
  let seqs = seqTypes sys
  ds <- mapM ((\(Let v) -> seq v) . (variables M.!)) seqs
  return (M.fromList $ zip (M.keys $ defs sys) ds)

mkDefinitions
  :: Integral a
  => System a
  -> Expr
  -> Map String Let
  -> Map String Def
  -> Spec [Let]
mkDefinitions sys z variables seqVariables = do
  xs <- mapM (mkDefinition z variables seqVariables) (M.assocs $ defs sys)
  return $ concat xs

mkDefinition
  :: Integral a
  => Expr
  -> Map String Let
  -> Map String Def
  -> (String, [Cons a])
  -> Spec [Let]
mkDefinition z variables seqVariables (s, constrs) = do
  let (Let v) = variables M.! s
  (expr, us) <- mkExpr z variables seqVariables constrs
  v .=. expr -- craft a type definition
  return [ u | Just u <- us ]

mkExpr
  :: Integral a
  => Expr
  -> Map String Let
  -> Map String Def
  -> [Cons a]
  -> Spec (Expr, [Maybe Let])
mkExpr z variables seqVariables cons = do
  xs <- mapM (toExpr z variables seqVariables) cons
  let expr = sum $ map fst xs
  return (expr, map snd xs)

toExpr
  :: Integral a
  => Expr
  -> Map String Let
  -> Map String Def
  -> Cons a
  -> Spec (Expr, Maybe Let)
toExpr z variables seqVariables cons = do
  u <- mkTuningVariable cons
  return (defaults u * z ^ w * product arg, u)
 where
  arg = map (toExpr' variables seqVariables) (args cons)
  w   = weight cons

  defaults Nothing        = 1
  defaults (Just (Let x)) = x

toExpr' :: Map String Let -> Map String Def -> Arg -> Expr
toExpr' variables _ (Type name) = let Let v = variables M.! name in v
toExpr' _ seqVariables (List name) = let Def v = seqVariables M.! name in v

mkTuningVariable :: Cons a -> Spec (Maybe Let)
mkTuningVariable cons = case frequency cons of
  Nothing -> return Nothing
  Just f  -> do
    u <- variable' (fromIntegral f)
    return $ Just u

tuneSystem :: Map String Let -> String -> Spec ()
tuneSystem variables target = tune t where Let t = variables M.! target

getValues :: Map String Let -> Spec [Maybe Double]
getValues variables = mapM (\(Let v) -> value v) (M.elems variables)

toDouble :: [Maybe a] -> [a]
toDouble = map fromJust

-- | Constructs a corresponding tuning problem for the given system.
--   The target variable (type to be generated) is expected as argument.
--   The respective size parameter is extracted from the system annotations.
toPaganini
  :: Integral a
  => String
  -> System a
  -> IO
       ( Either
           PaganiniError
           ( Double
           , [Double]
           , [Double]
           , Map String [Int]
           , Map String [Int]
           )
       )
toPaganini target sys = paganini $ do
  let n = structureSize sys
  Let z <- variable' $ fromIntegral n
  xs    <- mkVariables sys

  ds    <- mkSeqVariables sys xs
  us    <- mkDefinitions sys z xs ds

  tuneSystem xs target

  z'     <- value z
  xs'    <- getValues xs

  xsDDGs <- mapM
    (\(s, x) -> do
      x' <- ddg x
      return (s, fromJust x')
    )
    (M.toList xs)

  ysDDGs <- mapM
    (\(s, x) -> do
      x' <- ddg x
      return (s, fromJust x')
    )
    (M.toList ds)


  us' <- mapM (\(Let u) -> value u) us

  return
    ( fromJust z'
    , toDouble xs'
    , toDouble us'
    , M.fromList xsDDGs
    , M.fromList ysDDGs
    )
