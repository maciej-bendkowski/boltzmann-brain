{-|
 Module      : Data.Boltzmann.Compiler.Haskell.MaybeT
 Description : Boltzmann system compiler for ghc-7.10.3.
 Copyright   : (c) Maciej Bendkowski, 2017
 
 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.Compiler.Haskell.MaybeT
    ( Conf(..)
    , compile
    ) where

import Language.Haskell.Exts hiding (List)
import qualified Language.Haskell.Exts as LHE

import Language.Haskell.Exts.SrcLoc (noLoc)

import Data.Boltzmann.System

import Data.Boltzmann.Compiler
import Data.Boltzmann.Compiler.Haskell.Helpers

data Conf a = Conf { paramSys    :: PSystem a
                   , moduleName  :: String
                   , compileNote :: String
                   , withIO      :: Bool
                   }

instance (Real a, Show a) => Configuration (Conf a) where
    compile conf = let sys     = paramSys conf
                       name'   = moduleName conf
                       note    = compileNote conf
                       withIO' = withIO conf
                       module' = compileModule sys name' withIO'
                   in do
                       putStr $ moduleHeader sys note
                       putStrLn $ prettyPrint module'

moduleHeader :: Show a => PSystem a -> String -> String
moduleHeader sys compilerNote = unlines ["-- | Compiler: " ++ compilerNote,
                                         "-- | Singularity: " ++ show (param sys)]

compileModule :: Real a => PSystem a -> String -> Bool -> Module
compileModule sys mod' withIO' = Module noLoc (ModuleName mod') []
                                      Nothing (Just exports) imports decls
    where
        exports = declareExports sys withIO'
        imports = declareImports withIO'
        decls = declareADTs sys ++
                    declareGenerators sys ++
                    declareListGenerators sys ++
                    declareSamplers sys ++
                    declareListSamplers sys ++
                    declareSamplersIO sys withIO' ++
                    declareListSamplersIO sys withIO'

declareImports :: Bool -> [ImportDecl]
declareImports withIO' = 
    [importFrom "Control.Monad" [importFunc "guard"],
     importFrom "Control.Monad.Trans" [importFunc "lift"],
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

listSamplerName :: ShowS
listSamplerName t = samplerName t ++ "List"

samplerIOName :: ShowS
samplerIOName t = samplerName t ++ "IO"

listSamplerIOName  :: ShowS
listSamplerIOName t = listSamplerName t ++ "IO"

declareExports :: PSystem a -> Bool -> [ExportSpec]
declareExports sys withIO' =
    exportGenerators sys ++
    exportListGenerators sys ++
    exportSamplers sys ++
    exportListSamplers sys ++
    exportSamplersIO sys withIO' ++
    exportListSamplersIO sys withIO'

exportGenerators :: PSystem a -> [ExportSpec]
exportGenerators sys = map (exportFunc . genName) $ typeList sys

exportListGenerators :: PSystem a -> [ExportSpec]
exportListGenerators sys = map (exportFunc . listGenName) $ typeList sys

exportSamplers :: PSystem a -> [ExportSpec]
exportSamplers sys = map (exportFunc . samplerName) $ typeList sys

exportListSamplers :: PSystem a -> [ExportSpec]
exportListSamplers sys = map (exportFunc . listSamplerName) $ typeList sys

exportSamplersIO :: PSystem a -> Bool -> [ExportSpec]
exportSamplersIO _ False = []
exportSamplersIO sys True = map (exportFunc . samplerIOName) $ typeList sys

exportListSamplersIO :: PSystem a -> Bool -> [ExportSpec]
exportListSamplersIO _ False = []
exportListSamplersIO sys True = map (exportFunc . listSamplerIOName) $ typeList sys

-- Utils.
maybeT' :: Type
maybeT' = typeCons "MaybeT"

rand' :: Type
rand' = typeCons "Rand"

int' :: Type
int' = typeCons "Int"

g' :: Type
g' = typeVar "g"

randomGen' :: QName
randomGen' = unname "RandomGen"

return' :: Exp
return' = varExp "return"

nat :: [String]
nat = map show ([0..] :: [Integer])

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

randomP :: String -> Stmt
randomP v = bind v $ varExp "randomP"

guardian :: String -> Stmt
guardian v = Qualifier $ App (varExp "guard") 
                             (varExp v `greater` toLit 0) 

declareGenerators :: Real a => PSystem a -> [Decl]
declareGenerators sys = 
    declRandomP ++ 
        concatMap declGenerator (paramTypesW sys)

declGenerator :: Real a => (String, [(Cons a, Int)]) -> [Decl]
declGenerator (t, g) = declTFun (genName t) type' ["ub"] body
    where type' = generatorType $ typeCons t
          body  = constrGenerator g

constrGenerator :: Real a => [(Cons a, Int)] -> Exp
constrGenerator [(constr, w)] = rec constr w
constrGenerator cs = Do (initSteps ++ branching)
    where branching = [Qualifier $ constrGenerator' cs]
          initSteps = [guardian "ub", 
                       randomP "p"]
                    
constrGenerator' :: Real a => [(Cons a, Int)] -> Exp
constrGenerator' [(constr, w)] = rec constr w
constrGenerator' ((constr, w) : cs) =
    If (lessF (varExp "p") $ weight constr)
       (rec constr w)
       (constrGenerator' cs)
constrGenerator' _ = error "I wasn't expecting the spanish inquisition!"

rec :: Cons a -> Int -> Exp
rec constr w = case arguments (args constr) (toLit w) variableStream weightStream of
                 ([], _, _)          -> applyF return' [Tuple Boxed [cons, toLit w]]
                 (stmts, totalW, xs) -> Do (stmts ++ [ret cons xs (toLit w `add` totalW)])
    
    where cons = conExp $ func constr

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
arguments' _ _ _ _ _ _ = error "I wasn't expecting the spanish inquisition!"

ret :: Exp -> [Exp] -> Exp -> Stmt
ret f [] w = Qualifier $ applyF return' [Tuple Boxed [f, w]]
ret f xs w = Qualifier $ applyF return' [Tuple Boxed [t, w]]
    where t = applyF f xs

-- List generators.
listGeneratorType :: Type -> Type
listGeneratorType type' = TyForall Nothing 
    [ClassA randomGen' [g']]
        (TyFun int' (maybeTType $ TyTuple Boxed [TyList type', int']))

declareListGenerators :: Real a => PSystem a -> [Decl]
declareListGenerators sys = concatMap (declListGenerator sys) $ typeList sys

declListGenerator :: Real a => PSystem a -> String -> [Decl]
declListGenerator sys t = declTFun (listGenName t) type' ["ub"] body
    where type' = listGeneratorType (typeCons t)
          body  = constrListGenerator sys t

constrListGenerator :: Real a => PSystem a -> String -> Exp
constrListGenerator sys t = Do (initSteps ++ branching)
    where branching = [Qualifier $ constrListGenerator' sys t]
          initSteps = [guardian "ub", 
                       randomP "p"]

constrListGenerator' :: Real a => PSystem a -> String -> Exp
constrListGenerator' sys t = 
    If (lessF (varExp "p") (typeWeight sys t))
       (retHeadList t)
       retNil

retHeadList :: String -> Exp
retHeadList t = Do 
    [bindP "x" "w" (applyF (varExp $ genName t) [varExp "ub"]),
     bindP "xs" "ws" (applyF (varExp $ listGenName t) [varExp "ub" `sub` varExp "w"]),
     ret (InfixApp (varExp "x") (symbol ":") (varExp "xs")) 
            [] (varExp "w" `add` varExp "ws")]

retNil :: Exp
retNil = applyF return' [Tuple Boxed [LHE.List [], toLit 0]]

-- Samplers.
samplerType :: Type -> Type
samplerType type' = TyForall Nothing 
    [ClassA randomGen' [g']]
    (TyFun int' 
           (TyFun int'
                  (TyApp (TyApp rand' g') type')))

declareSamplers :: PSystem a -> [Decl]
declareSamplers sys = concatMap declSampler $ typeList sys

declSampler :: String -> [Decl]
declSampler t = declTFun (samplerName t) type' ["lb","ub"] body
    where type' = samplerType (typeCons t)
          body  = constructSampler t

constructSampler' :: (t -> String) -> (t -> String) -> t -> Exp
constructSampler' gen sam t =
    Do [bind "sample" (applyF (varExp "runMaybeT") 
            [applyF (varExp $ gen t) [varExp "ub"]]),
            caseSample] 
    where caseSample = Qualifier $ Case (varExp "sample") 
                 [Alt noLoc (PApp (unname "Nothing") [])
                        (UnGuardedRhs rec') Nothing,
                        Alt noLoc (PApp (unname "Just") 
                 [PTuple Boxed [PVar $ Ident "x",
                  PVar $ Ident "s"]])
                  (UnGuardedRhs return'') Nothing]

          rec' = applyF (varExp $ sam t) [varExp "lb", varExp "ub"]
          return'' = If (lessEq (varExp "lb") (varExp "s"))
                        (applyF (varExp "return") [varExp "x"])
                        rec'

constructSampler :: String -> Exp
constructSampler = constructSampler' genName samplerName

declareListSamplers :: PSystem a -> [Decl]
declareListSamplers sys = concatMap declListSampler $ typeList sys

declListSampler :: String -> [Decl]
declListSampler t = declTFun (listSamplerName t) type' ["lb","ub"] body
    where type' = samplerType (TyList $ typeCons t)
          body  = constructListSampler t

constructListSampler :: String -> Exp
constructListSampler = constructSampler' listGenName listSamplerName

-- IO Samplers.
samplerIOType :: Type -> Type
samplerIOType type' = TyForall Nothing 
    [] (TyFun int' (TyFun int' (TyApp (typeVar "IO") type')))

declareSamplersIO :: PSystem a -> Bool -> [Decl]
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

declareListSamplersIO :: PSystem a -> Bool -> [Decl]
declareListSamplersIO _ False = []
declareListSamplersIO sys True = concatMap declListSamplerIO $ typeList sys

declListSamplerIO :: String -> [Decl]
declListSamplerIO t = declTFun (listSamplerIOName t) type' ["lb","ub"] body
    where type' = samplerIOType (TyList $ typeCons t)
          body  = constructListSamplerIO t

constructListSamplerIO :: String -> Exp
constructListSamplerIO = constructSamplerIO' listSamplerName
