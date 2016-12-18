-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Compiler.Haskell.MaybeT
    ( Configuration(..)
    , compile
    ) where

import Language.Haskell.Exts
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (noLoc)

import System
import System.Boltzmann

import Compiler
import Compiler.Haskell.Helpers

data Configuration b a = Configuration { paramSys :: BoltzmannSystem b a
                                       , moduleName :: String
                                       , compileNote :: String
                                       , withIO :: Bool
                                       }

instance (Real a, Show a) => Compilable (Configuration b a) where
    compile conf = let sys = paramSys conf
                       name = moduleName conf
                       note = compileNote conf
                       withIO' = withIO conf
                       module' = compileModule sys name withIO'
                   in do
                       putStr $ moduleHeader sys note
                       putStrLn $ prettyPrint module'

moduleHeader :: Show a => BoltzmannSystem b a -> String -> String
moduleHeader sys compilerNote = unlines ["-- | Compiler: " ++ compilerNote,
                                         "-- | Singularity: " ++ show (parameter sys)]

compileModule sys mod withIO = Module noLoc (ModuleName mod) []
                                      Nothing (Just exports) imports decls
    where
        exports = declareExports sys withIO
        imports = declareImports withIO
        decls = declareADTs sys ++
                    declareGenerators sys ++
                    declareSamplers sys ++
                    declareSamplersIO sys withIO

declareImports withIO = 
    [importFrom "Control.Monad" [importFunc "guard"],
     importFrom "Control.Monad.Trans" [importFunc "lift"],
     importFrom "Control.Monad.Trans.Maybe" [importType "MaybeT",
                                             importFunc "runMaybeT"],
     
     importFrom "Control.Monad.Random" ([importType "RandomGen",
                                        importFunc "Rand",
                                        importFunc "getRandomR"]
                                        ++ importIO withIO)]

importIO False = []
importIO True = [importFunc "evalRandIO"]

-- Naming functions.
genName = (++) "genRandom"
samplerName = (++) "sample"
samplerIOName t = samplerName t ++ "IO"

declareExports sys withIO =
    exportGenerators sys ++
    exportSamplers sys ++
    exportSamplersIO sys withIO

exportGenerators sys = map (exportFunc . genName) $ typeList sys
exportSamplers sys = map (exportFunc . samplerName) $ typeList sys

exportSamplersIO sys False = []
exportSamplersIO sys True = map (exportFunc . samplerIOName) $ typeList sys

-- Utils.
maybeT' = typeCons "MaybeT"
rand' = typeCons "Rand"
int' = typeCons "Int"
g' = typeVar "g"

randomGen' = unname "RandomGen"
return' = varExp "return"

nat :: [String]
nat = map show [0..]

variableStream :: [String]
variableStream = map ('x' :) nat

weightStream :: [String]
weightStream = map ('w' :) nat

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

randomP v = bind v $ varExp "randomP"
guardian v = Qualifier $ App (varExp "guard") 
                             (varExp v `greater` toLit 0) 

declareGenerators sys = 
    declRandomP ++ 
        concatMap declGenerator (paramTypes' sys)

declGenerator (t, g @ cons) = declTFun (genName t) type' ["ub"] body
    where type' = generatorType $ typeCons t
          body = constrGenerator g 

constrGenerator [(constr, w)] = rec constr w
constrGenerator cs = Do (initSteps ++ branching)
    where branching = [Qualifier $ constrGenerator' cs]
          initSteps = [guardian "ub", 
                       randomP "p"]
                    
constrGenerator' [(constr, w)] = rec constr w
constrGenerator' ((constr, w) : cs) =
    If (lessF (varExp "p") $ weight constr)
       (rec constr w)
       (constrGenerator' cs)

rec constr w = case arguments (args constr) (toLit w) variableStream weightStream of
                 ([], _, _) -> applyF return' [Tuple Boxed [cons, toLit w]]
                 (stmts, totalW, xs) -> Do (stmts ++ [ret cons xs (toLit w `add` totalW)])
    
    where cons = conExp $ func constr

arguments [] ub _ _ = ([], toLit 0, [])
arguments (Type arg:args) ub (x:xs) (w:ws) = (stmt : stmts, argW', v : vs)
    where stmt = bindP x w $ applyF (varExp $ genName arg) [varExp "ub" `sub` ub]
          (stmts, argW, vs) = arguments args ub' xs ws
          argW' = argW `add` varExp w
          ub' = ub `sub` varExp w
          v = varExp x

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

declareSamplers sys = concatMap declSampler $ typeList sys

declSampler :: String -> [Decl]
declSampler t = declTFun (samplerName t) type' ["lb","ub"] body
    where type' = samplerType (typeCons t) 
          body = constructSampler t

constructSampler t =
    Do [bind "sample" (applyF (varExp "runMaybeT") 
            [applyF (varExp $ genName t) [varExp "ub"]]),
            caseSample t] 
    where caseSample t = Qualifier $ Case (varExp "sample") 
                 [Alt noLoc (PApp (unname "Nothing") [])
                        (UnGuardedRhs rec') Nothing,
                        Alt noLoc (PApp (unname "Just") 
                 [PTuple Boxed [PVar $ Ident "x",
                  PVar $ Ident "s"]])
                  (UnGuardedRhs return') Nothing]

          rec' = applyF (varExp $ samplerName t) [varExp "lb", varExp "ub"]
          return' = If (lessEq (varExp "lb") (varExp "s"))
                      (applyF (varExp "return") [varExp "x"])
                      rec'

-- IO Samplers.
samplerIOType :: Type -> Type
samplerIOType type' = TyForall Nothing 
    [] (TyFun int' (TyFun int' (TyApp (typeVar "IO") type')))

declareSamplersIO sys False = []
declareSamplersIO sys True = concatMap declSamplerIO $ typeList sys

declSamplerIO :: String -> [Decl]
declSamplerIO t = declTFun (samplerIOName t) type' ["lb","ub"] body
    where type' = samplerIOType (typeCons t) 
          body = constructSamplerIO t

constructSamplerIO t = applyF (varExp "evalRandIO")
                              [applyF (varExp $ samplerName t) [varExp "lb",
                                                                varExp "ub"]]
