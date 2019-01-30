{-|
 Module      : Data.Boltzmann.Compiler.Haskell.Matrix
 Description : Rational Boltzmann system compiler for ghc-7.10.3.
 Copyright   : (c) Maciej Bendkowski, 2017-2019

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 Transition matrix system compiler for rational specifications.
 The outcome sampler is a rejection-based sampler implementing the
 interruptible sampling scheme for strongly connected specifications.
 Internally, the system is represented as a adjacency-list graph with
 additional labels on edges (transition letters).
 -}
module Data.Boltzmann.Compiler.Haskell.Matrix
    ( Conf(..)
    , compile
    , config
    ) where

import Prelude hiding (and)

import Language.Haskell.Exts hiding (List,Cons)
import qualified Language.Haskell.Exts as LHE
import Language.Haskell.Exts.SrcLoc (noLoc)

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Data.Maybe (fromMaybe)

import Data.Boltzmann.System
import Data.Boltzmann.Internal.Annotations

import Data.Boltzmann.Compiler
import Data.Boltzmann.Compiler.Haskell.Helpers

-- | Default configuration type.
data Conf = Conf { paramSys    :: PSystem Double   -- ^ Parametrised system.
                 , moduleName  :: String           -- ^ Module name.
                 , compileNote :: String           -- ^ Header comment note.
                 , withIO      :: Bool             -- ^ Generate IO actions?
                 }

instance Configuration Conf where

    config sys module' compilerNote' =
         let with = withBool (annotations $ system sys)
         in Conf { paramSys    = sys
                 , moduleName  = module'
                 , compileNote = compilerNote'
                 , withIO      = "withIO" `with` True
                 }

    compile conf = let sys        = paramSys conf
                       name'      = moduleName conf
                       note       = compileNote conf
                       withIO'    = withIO conf
                       module'    = compileModule sys name' withIO'
                   in do
                       putStr $ moduleHeader sys note
                       putStrLn $ prettyPrint module'

moduleHeader :: PSystem Double -> String -> String
moduleHeader sys compilerNote =
    unlines (["-- | Compiler: " ++ compilerNote,
              "-- | Singularity: " ++ show (param sys),
              "-- | System type: rational",
              "-- | Stability: experimental"] ++ systemNote sys)

compileModule :: PSystem Double -> String -> Bool -> Module
compileModule sys mod' withIO' =
    Module noLoc (ModuleName mod')
        [LanguagePragma noLoc [Ident "TemplateHaskell"]]
        Nothing (Just $ declareExports withIO')
            (declareImports withIO') (decls sys withIO')

declareExports :: Bool -> [ExportSpec]
declareExports withIO' =
    exportFunc "sampleWord"
        : exportFunc "startingState"
        : [exportFunc "sampleWordIO" | withIO']

declareImports :: Bool -> [ImportDecl]
declareImports withIO' =
    [importFrom "Control.Monad.Trans" [importFunc "lift"],
     importFrom "Control.Monad.Trans.Maybe" [importType "MaybeT",
                                             importFunc "runMaybeT"],

    importFrom "Data.Buffon.Machine" ([importType "BuffonMachine",
                                        importType "DecisionTree",
                                        importFunc "decisionTree",
                                        importFunc "choice"]
                                        ++ importIO withIO'),

     importQual "Language.Haskell.TH.Syntax" "TH",

     importFrom "System.Random" [importType "RandomGen"]]

importIO :: Bool -> [ImportSpec]
importIO withIO' = [importFunc "runRIO" | withIO']

decls :: PSystem Double -> Bool -> [Decl]
decls sys withIO' = symbolDecl :
                    declWeight sys
                    ++ declSymb sys
                    ++ declDecisionTrees sys
                    ++ declTerminals sys
                    ++ declGraph sys
                    ++ declGen
                    ++ declSampler
                    ++ declStartingState sys
                    ++ concat [declSamplerIO | withIO']

-- | Type synonym for alphabet letters.
symbolDecl :: Decl
symbolDecl = TypeDecl noLoc (Ident "Symbol") []
    (TyCon $ unname "String")

-- | Converts the given system into
--   corresponding a graph representation.
toGraph :: PSystem Double -> [[(Int, Int)]]
toGraph sys = map (typeAdj $ types sys') typs
    where typs = M.toList (defs sys')
          sys' = system sys
          lts  = alphabet sys'

          typeAdj typs' (_, cons) = map (consAdj typs') cons

          -- | Assigns the given constructor a reference index,
          --   pointing to the following node (type).
          typeIdx typs' con
              | isAtomic con = -1 -- note: epsilon transition.
              | otherwise = let typ = argName (head $ args con)
                                  in typ `S.findIndex` typs'

          consAdj typs' con = (n, w)
              where a = func con -- note: that's in fact the transition letter.
                    n = typeIdx typs' con
                    w = fromMaybe (-1) (a `lookupLetter` lts)

buffonMachineType :: Type
buffonMachineType = typeCons "BuffonMachine"

maybeTType :: Type -> Type
maybeTType = TyApp (TyApp maybeT' (TyApp buffonMachineType g'))

letters :: PSystem Double -> [Letter]
letters = S.toList . alphabet . system

getWeights :: Int -> [Letter] -> [(Int, Rhs)]
getWeights n (s : xs) = (n, UnGuardedRhs $ toLit (weightL s)) : xs'
    where xs' = getWeights (succ n) xs

getWeights _ _ = []

getSymbols :: Int -> [Letter] -> [(Int, Rhs)]
getSymbols n (s : xs) = (n, UnGuardedRhs $ toString (symb s)) : xs'
    where xs' = getSymbols (succ n) xs

getSymbols _ _ = []

-- | Symbol weights.
declWeight :: PSystem Double -> [Decl]
declWeight sys = declTFun "weight" type' ["n"] body
    where type' = TyFun int' int'
          body  = caseInt "n" $ getWeights 0 (letters sys)

-- | Symbol strings.
declSymb :: PSystem Double -> [Decl]
declSymb sys = declTFun "symbol" type' ["n"] body
    where type' = TyFun int' (typeCons "Symbol")
          body  = caseInt "n" $ getSymbols 0 (letters sys)

getDecisionTree :: (String, [(Cons Double, Int)]) -> Exp
getDecisionTree (_, g) = spliceExp lift'
    where lift' = applyF (qVarExp "TH" "lift") [dt']
          dt'   = applyF (varExp "decisionTree") [prob]
          prob  = LHE.List (init $ probList g)

getDecitionTrees :: Int -> [(String, [(Cons Double, Int)])] -> [(Int, Rhs)]
getDecitionTrees n (s : xs) = (n, UnGuardedRhs $ getDecisionTree s) : xs'
    where xs' = getDecitionTrees (succ n) xs

getDecitionTrees _ _ = []

declDecisionTrees :: PSystem Double -> [Decl]
declDecisionTrees sys = declTFun "decisionTrees" type' ["s"] body
    where type' = TyFun int' decisionTreeType
          body = caseInt "s" $ getDecitionTrees 0 (paramTypesW sys)

getTerminals :: Int -> [(String, [(Cons Double, Int)])] -> [(Int, Rhs)]
getTerminals n (s : xs) = (n, UnGuardedRhs $ getTerminal s) : xs'
    where xs' = getTerminals (succ n) xs

getTerminals _ _ = []

getTerminal :: (String, [(Cons Double, Int)]) -> Exp
getTerminal (_, g)
    | any (isAtomic . fst) g = conExp "True"
    | otherwise = conExp "False"

declTerminals :: PSystem Double -> [Decl]
declTerminals sys = declTFun "isTerminal" type' ["s"] body
    where type' = TyForall Nothing [] (TyFun int' (typeCons "Bool"))
          body = caseInt "s" $ getTerminals 0 (paramTypesW sys)

declGraph :: PSystem Double -> [Decl]
declGraph sys = declTFun "transitionMatrix" type' ["n", "m"] body
    where type' = TyForall Nothing [] (TyFun int'
                        (TyFun int' (TyTuple Boxed [int', int'])))

          graph = toGraph sys
          body = caseInt "n" [(i, UnGuardedRhs $ getNeighbourhood (graph !! i))
                                                    | i <- [0..pred (length graph)]]

getNeighbourhood :: [(Int, Int)] -> Exp
getNeighbourhood xs =
    caseInt "m" (getNeighbourhood' 0 xs)

getNeighbourhood' :: Int -> [(Int, Int)] -> [(Int, Rhs)]
getNeighbourhood' n ((k,w) : xs) = (n, UnGuardedRhs p) : xs'
    where xs' = getNeighbourhood' (succ n) xs
          p = Tuple Boxed [toLit k, toLit w]

getNeighbourhood' _ _ = []

declGen :: [Decl]
declGen = declTFun "genWord" type' ["ub", "s"] mainIfStmt
    where type' = TyForall Nothing [ClassA randomGen' [g']]
            (TyFun int' $ TyFun int' (maybeTType $ TyTuple Boxed
                [TyList $ typeCons "Symbol", int']))

          mainIfStmt = If (varExp "ub" `lessEq` toLit 0
                            `and` applyF (varExp "isTerminal") [varExp "s"])
                         (App return' (Tuple Boxed [LHE.List [], toLit 0]))
                         mainBody

          mainBody = Do [ choiceStmt
                        , getNext
                        , ifStmt]

          choiceStmt = choiceN "n" (applyF (varExp "decisionTrees") [varExp "s"])

          getNext = LetStmt (BDecls [getNext'])
          getNext' = PatBind noLoc
                        (PTuple Boxed [PVar $ Ident "s'", PVar $ Ident "i"])
                        (UnGuardedRhs $ applyF (varExp "transitionMatrix")
                                            [varExp "s", varExp "n"]) Nothing

          ifStmt = Qualifier $ If (less (varExp "s'") (toLit 0))
                      (App return' (Tuple Boxed [LHE.List [], toLit 0])) elseStmt

          elseStmt = Do [ bindSymbol
                        , recursiveCall
                        , returnStmt]

          bindSymbol = LetStmt (BDecls [bindSymbol'])
          bindSymbol' = PatBind noLoc
                            (PTuple Boxed [PVar $ Ident "a", PVar $ Ident "w"])
                        (UnGuardedRhs $
                            Tuple Boxed [ applyF (varExp "symbol") [varExp "i"]
                                        , applyF (varExp "weight") [varExp "i"]])
                                             Nothing

          recursiveCall = Generator noLoc
                            (PTuple Boxed [PVar $ Ident "as", PVar $ Ident "w'"])
                            (App (App (varExp "genWord")
                                (varExp "ub" `sub` varExp "w"))
                                (varExp "s'"))

          returnStmt = Qualifier $ App return'
                        (Tuple Boxed
                            [InfixApp (varExp "a") (symbol ":") (varExp "as")
                            ,varExp "w" `add` varExp "w'"])

declSampler :: [Decl]
declSampler = declTFun "sampleWord" type' ["lb", "ub", "s"] constructSampler
    where type' = TyForall Nothing [ClassA randomGen' [g']]
            (TyFun int' $ TyFun int' $ TyFun int' $ TyApp (TyApp buffonMachineType g')
                (TyList $ typeCons "Symbol"))

constructSampler :: Exp
constructSampler =
    Do [bind "str" (applyF (varExp "runMaybeT")
            [applyF (varExp "genWord") [varExp "lb", varExp "s"]]),
            caseSample]
    where caseSample = Qualifier $ Case (varExp "str")
                 [Alt noLoc (PApp (unname "Nothing") [])
                        (UnGuardedRhs rec') Nothing,
                        Alt noLoc (PApp (unname "Just")
                 [PTuple Boxed [PVar $ Ident "w",
                  PVar $ Ident "n"]])
                  (UnGuardedRhs return'') Nothing]

          rec' = applyF (varExp "sampleWord") [varExp "lb", varExp "ub", varExp "s"]
          return'' = If (lessEq (varExp "lb") (varExp "n") `and` lessEq (varExp "n") (varExp "ub"))
                        (applyF (varExp "return") [varExp "w"])
                        rec'

declSamplerIO :: [Decl]
declSamplerIO = declTFun "sampleWordIO" type' ["lb","ub", "s"] body
    where body  = constructSamplerIO
          type' = TyForall Nothing
                    [] (TyFun int' (TyFun int'
                       (TyFun int'
                       (TyApp (typeVar "IO")
                            (TyList $ typeCons "Symbol")))))

constructSamplerIO :: Exp
constructSamplerIO = applyF (varExp "runRIO")
                               [applyF (varExp "sampleWord")
                                [varExp "lb"
                                ,varExp "ub"
                                ,varExp "s"]]

-- | Finds the starting state for the sampler.
startingState :: PSystem Double -> Int
startingState sys =
    gen `S.findIndex` types sys'
    where sys' = system sys
          ann = annotations sys'
          gen = withString ann "generate" (initType sys')

declStartingState :: PSystem Double -> [Decl]
declStartingState sys = declTFun "startingState" type' [] body
    where body = toLit (startingState sys)
          type' = TyForall Nothing [] int'
