{-|
 Module      : Data.Boltzmann.Compiler.Haskell.Rational
 Description : Rational Boltzmann system compiler for ghc-7.10.3.
 Copyright   : (c) Maciej Bendkowski, 2017-2019

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 Rational system compiler using Haskell's built-in algebraic data types
 to handle to given system types. The outcome sampler is a rejection-based
 sampler implementing the interruptible sampling scheme for strongly connected
 specifications.
 -}
module Data.Boltzmann.Compiler.Haskell.Rational
    ( Conf(..)
    , compile
    , config
    ) where

import Prelude hiding (and)
import Language.Haskell.Exts hiding (List)
import Language.Haskell.Exts.SrcLoc (noLoc)

import Data.Boltzmann.System
import Data.Boltzmann.Internal.Annotations

import Data.Boltzmann.Compiler
import Data.Boltzmann.Compiler.Haskell.Helpers

-- | Default configuration type.
data Conf = Conf { paramSys    :: PSystem Double   -- ^ Parametrised system.
                 , moduleName  :: String           -- ^ Module name.
                 , compileNote :: String           -- ^ Header comment note.
                 , withIO      :: Bool             -- ^ Generate IO actions?
                 , withShow    :: Bool             -- ^ Generate deriving Show?
                 }

instance Configuration Conf where

    config sys module' compilerNote' =
        let with = withBool (annotations $ system sys)
         in Conf { paramSys    = sys
                 , moduleName  = module'
                 , compileNote = compilerNote'
                 , withIO      = "withIO"    `with` True
                 , withShow    = "withShow"  `with` True
                 }

    compile conf = let sys        = paramSys conf
                       name'      = moduleName conf
                       note       = compileNote conf
                       withIO'    = withIO conf
                       withShow'  = withShow conf
                       module'    = compileModule sys name'
                                        withIO' withShow'
                   in do
                       putStr $ moduleHeader sys note
                       putStrLn $ prettyPrint module'

moduleHeader :: PSystem Double -> String -> String
moduleHeader sys compilerNote =
    unlines (["-- | Compiler: " ++ compilerNote,
              "-- | Singularity: " ++ show (param sys),
              "-- | System type: rational",
              "-- | Stability: experimental"] ++ systemNote sys)

compileModule :: PSystem Double -> String -> Bool -> Bool -> Module
compileModule sys mod' withIO' withShow' =
    Module noLoc (ModuleName mod') []
        Nothing (Just exports) imports decls
    where
        exports = declareExports sys withIO'
        imports = declareImports withIO'
        decls = declareADTs withShow' sys ++
                    declareGenerators sys ++
                    declareSamplers sys ++
                    declareSamplersIO sys withIO'

declareImports :: Bool -> [ImportDecl]
declareImports withIO' =
    [importFrom "Control.Monad.Trans" [importFunc "lift"],
     importFrom "Control.Monad.Trans.Maybe" [importType "MaybeT",
                                             importFunc "runMaybeT"],

     importFrom "Control.Monad.Random" ([importType "RandomGen",
                                        importFunc "Rand",
                                        importFunc "getRandomR"]
                                        ++ importIO withIO')]

importIO :: Bool -> [ImportSpec]
importIO False = []
importIO True  = [importFunc "evalRandIO"]

-- Naming functions.
genName :: ShowS
genName = (++) "genRandom"

listGenName :: ShowS
listGenName t = genName t ++ "List"

samplerName :: ShowS
samplerName = (++) "sample"

samplerIOName :: ShowS
samplerIOName t = samplerName t ++ "IO"

declareExports :: PSystem Double -> Bool -> [ExportSpec]
declareExports sys withIO' =
    exportTypes sys ++
    exportGenerators sys ++
    exportSamplers sys ++
    exportSamplersIO sys withIO'

exportGenerators :: PSystem Double -> [ExportSpec]
exportGenerators sys = map (exportFunc . genName) $ typeList sys

exportSamplers :: PSystem Double -> [ExportSpec]
exportSamplers sys = map (exportFunc . samplerName) $ typeList sys

exportSamplersIO :: PSystem Double -> Bool -> [ExportSpec]
exportSamplersIO _ False = []
exportSamplersIO sys True = map (exportFunc . samplerIOName) $ typeList sys

-- Utils.
rand' :: Type
rand' = typeCons "Rand"

-- Generators.
maybeTType :: Type -> Type
maybeTType = TyApp (TyApp maybeT' (TyApp rand' g'))

generatorType :: Type -> Type
generatorType type' = TyForall Nothing
    [ClassA randomGen' [g']]
    (TyFun int' (maybeTType $ TyTuple Boxed [type', int']))

declRandomP :: [Decl]
declRandomP = declTFun "randomP" type' [] body
    where type' = TyForall Nothing [ClassA randomGen' [g']] (maybeTType $ typeVar "Double")
          body = App (varExp "lift")
                     (App (varExp "getRandomR")
                          (Tuple Boxed [toLit 0, toLit 1]))

randomP :: String -> Stmt
randomP v = bind v $ varExp "randomP"

when :: String -> (Cons Double, Int) -> Exp -> Stmt
when v (cons, w) exp' =
    Qualifier $ If (varExp v `lessEq` toLit 0)
                    (applyF return' [Tuple Boxed [conExp (func cons), toLit w]])
                    exp'

declareGenerators :: PSystem Double -> [Decl]
declareGenerators sys =
    declRandomP ++
        concatMap declGenerator (paramTypesW sys)

declGenerator :: (String, [(Cons Double, Int)]) -> [Decl]
declGenerator (t, g) = declTFun (genName t) type' ["ub"] body
    where type' = generatorType $ typeCons t
          body  = constrGenerator g

atoms :: [(Cons Double, Int)] -> [(Cons Double, Int)]
atoms = filter (isAtomic . fst)

constrGenerator :: [(Cons Double, Int)] -> Exp
constrGenerator [(constr, w)] = rec constr w
constrGenerator cs = Do initSteps
    where branching = [Qualifier $ constrGenerator' cs]
          terms     = atoms cs
          mainBody  = randomP "p" : branching
          initSteps = if length terms == 1 then [when "ub" (head terms) (Do mainBody)]
                                           else mainBody

constrGenerator' :: [(Cons Double, Int)] -> Exp
constrGenerator' [(constr, w)] = rec constr w
constrGenerator' ((constr, w) : cs) =
    If (lessF (varExp "p") $ weight constr)
       (rec constr w)
       (constrGenerator' cs)
constrGenerator' _ = error "I wasn't expecting the Spanish inquisition!"

rec :: Cons Double -> Int -> Exp
rec constr w =
    case arguments (args constr) (toLit w) variableStream weightStream of
      ([], _, _)          -> applyF return' [Tuple Boxed [conExp (func constr), toLit w]]
      (stmts, totalW, xs) ->
          let mainBody = stmts ++ [ret (conExp $ func constr) xs (toLit w `add` totalW)]
              interrupt = if isAtomic constr then [when "ub" (constr,w) (Do mainBody)]
                                             else mainBody
            in Do interrupt


arguments :: [Arg] -> Exp -> [String] -> [String] -> ([Stmt], Exp, [Exp])
arguments [] _ _ _ = ([], toLit 0, [])
arguments (Type arg:args') ub xs ws = arguments' genName arg args' ub xs ws
arguments (List arg:args') ub xs ws = arguments' listGenName arg args' ub xs ws

arguments' :: (t -> String) -> t -> [Arg] -> Exp -> [String] -> [String] -> ([Stmt], Exp, [Exp])
arguments' f arg args' ub (x:xs) (w:ws) = (stmt : stmts, argW', v : vs)
    where stmt              = bindP x w $ applyF (varExp $ f arg) [varExp "ub" `sub` ub]
          (stmts, argW, vs) = arguments args' ub' xs ws
          argW'             = argW `add` varExp w
          ub'               = ub `sub` varExp w
          v                 = varExp x
arguments' _ _ _ _ _ _ = error "I wasn't expecting the Spanish inquisition!"

ret :: Exp -> [Exp] -> Exp -> Stmt
ret f [] w = Qualifier $ applyF return' [Tuple Boxed [f, w]]
ret f xs w = Qualifier $ applyF return' [Tuple Boxed [t, w]]
    where t = applyF f xs

-- Samplers.
samplerType :: Type -> Type
samplerType type' = TyForall Nothing
    [ClassA randomGen' [g']]
    (TyFun int'
           (TyFun int'
                  (TyApp (TyApp rand' g') type')))

declareSamplers :: PSystem Double -> [Decl]
declareSamplers sys = concatMap declSampler $ typeList sys

declSampler :: String -> [Decl]
declSampler t = declTFun (samplerName t) type' ["lb","ub"] body
    where type' = samplerType (typeCons t)
          body  = constructSampler t

constructSampler' :: (t -> String) -> (t -> String) -> t -> Exp
constructSampler' gen sam t =
    Do [bind "sample" (applyF (varExp "runMaybeT")
            [applyF (varExp $ gen t) [varExp "lb"]]),
            caseSample]
    where caseSample = Qualifier $ Case (varExp "sample")
                 [Alt noLoc (PApp (unname "Nothing") [])
                        (UnGuardedRhs rec') Nothing,
                        Alt noLoc (PApp (unname "Just")
                 [PTuple Boxed [PVar $ Ident "x",
                  PVar $ Ident "s"]])
                  (UnGuardedRhs return'') Nothing]

          rec' = applyF (varExp $ sam t) [varExp "lb", varExp "ub"]
          return'' = If (lessEq (varExp "lb") (varExp "s") `and` lessEq (varExp "s") (varExp "ub"))
                        (applyF (varExp "return") [varExp "x"])
                        rec'

constructSampler :: String -> Exp
constructSampler = constructSampler' genName samplerName

-- IO Samplers.
samplerIOType :: Type -> Type
samplerIOType type' = TyForall Nothing
    [] (TyFun int' (TyFun int' (TyApp (typeVar "IO") type')))

declareSamplersIO :: PSystem Double -> Bool -> [Decl]
declareSamplersIO _ False = []
declareSamplersIO sys True = concatMap declSamplerIO $ typeList sys

declSamplerIO :: String -> [Decl]
declSamplerIO t = declTFun (samplerIOName t) type' ["lb","ub"] body
    where type' = samplerIOType (typeCons t)
          body  = constructSamplerIO t

constructSamplerIO' :: (t -> String) -> t -> Exp
constructSamplerIO' sam t = applyF (varExp "evalRandIO")
                               [applyF (varExp $ sam t) [varExp "lb",
                                                         varExp "ub"]]

constructSamplerIO :: String -> Exp
constructSamplerIO = constructSamplerIO' samplerName
