-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Compiler
    ( compile 
    ) where

import Language.Haskell.Exts
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (noLoc)

import System
import BoltzmannSystem

unname :: String -> QName
unname = UnQual . Ident

genName :: String -> String
genName = (++) "genRandom"

samplerName :: String -> String
samplerName = (++) "sample"

declTFun :: String -> Type -> [String] -> Exp -> [Decl]
declTFun f type' args body = [decl, FunBind [main]]
    where decl = TypeSig noLoc [Ident f] type'
          args' = map (PVar . Ident) args
          main = Match noLoc (Ident f) args' Nothing
                       (UnGuardedRhs body) Nothing

declPatternFun :: String -> [([Pat], Exp)] -> Decl
declPatternFun f cases = FunBind body
    where body = map match' cases
          match' (pat,b) = Match noLoc (Ident f) pat Nothing
                           (UnGuardedRhs b) Nothing

maybeTType :: Type -> Type
maybeTType = TyApp (TyApp (TyCon $ unname "MaybeT") 
                            (TyApp (TyCon $ unname "Rand") (TyVar $ Ident "g")))

randGenType :: String -> Type
randGenType typeName = TyForall Nothing 
    [ClassA (unname "RandomGen") [TyVar $ Ident "g"]]
    (maybeTType $ TyCon (unname typeName))

generatorType :: String -> Type
generatorType typeName = TyForall Nothing 
    [ClassA (unname "RandomGen") [TyVar $ Ident "g"]]
    (TyFun (TyCon $ unname "Int")
           (maybeTType $ TyTuple Boxed [TyCon $ unname typeName,
                                        TyCon $ unname "Int"]))

samplerType :: String -> Type
samplerType typeName = TyForall Nothing 
    [ClassA (unname "RandomGen") [TyVar $ Ident "g"]]
    (TyFun (TyCon $ unname "Int") 
           (TyFun (TyCon $ unname "Int") 
                  (TyApp (TyApp (TyCon (unname "Rand")) (TyVar (Ident "g")))
                  (TyCon (unname typeName)))))

compileModule :: Real a => BoltzmannSystem a -> String -> Module
compileModule sys moduleName = Module noLoc (ModuleName moduleName) []
                                      Nothing (Just exports) imports decls
    where
        exports = compileExports sys
        imports = compileImports
        decls = compileDecls sys

compileExports :: Real a => BoltzmannSystem a -> [ExportSpec]
compileExports sys = concatMap toADT $ typeList sys
    where toADT t = [EThingAll (unname t),
                     EVar (unname $ genName t),
                     EVar (unname $ samplerName t)]

compileImports :: [ImportDecl]
compileImports = [import' "Control.Monad" ctrlMonadS,
                  import' "Control.Monad.Random" ctrlRandS,
                  import' "Control.Monad.Trans" ctrlTransS,
                  import' "Control.Monad.Trans.Maybe" ctrlMaybeTS]
    where
        ctrlMonadS = Just (False, [IVar $ Ident "guard"])
        ctrlTransS = Just (False, [IVar $ Ident "lift"])
        ctrlMaybeTS = Just (False, [IThingAll $ Ident "MaybeT",
                                    IVar $ Ident "runMaybeT"])
        ctrlRandS = Just (False, [IThingAll $ Ident "RandomGen",
                                  IThingAll $ Ident "Rand",
                                  IVar $ Ident "getRandomR"])

import' name specs = ImportDecl { importLoc = noLoc
                                , importModule = ModuleName name
                                , importQualified = False
                                , importSrc = False
                                , importSafe = False
                                , importPkg = Nothing
                                , importAs = Nothing
                                , importSpecs = specs
                                }

compileDecls :: Real a => BoltzmannSystem a -> [Decl]
compileDecls sys = declADTs sys ++ declRandGen 
                ++ declGenerators sys ++ declSamplers sys

declSamplers :: Real a => BoltzmannSystem a -> [Decl]
declSamplers sys = concatMap declSampler $ typeList sys

declSampler :: String -> [Decl]
declSampler t = declTFun (samplerName t) type' ["lb","ub"] body
    where type' = samplerType t 
          body = constructSampler t

constructSampler :: String -> Exp
constructSampler t = Do [runMaybeT', runSampler', case']
    where
        runMaybeT' = 
            LetStmt $ BDecls [PatBind noLoc (PVar $ Ident "sampler")
                             (UnGuardedRhs $ App (Var $ unname "runMaybeT")
                                            (App (Var $ unname (genName t))
                                            (Var $ unname "ub")))
                                            Nothing]
        runSampler' =
            Generator noLoc (PVar $ Ident "x") (Var $ unname "sampler")

        rec' = App (App (Var $ unname (samplerName t))
                        (Var $ unname "lb")) (Var $ unname "ub")

        case' = Qualifier $
            Case (Var $ unname "x") [Alt noLoc (PApp (unname "Nothing") [])
                                         (UnGuardedRhs rec') Nothing,
                                    Alt noLoc (PApp (unname "Just") 
                                        [PTuple Boxed [PVar $ Ident "t'",
                                                       PVar $ Ident "s"]])
                                        (UnGuardedRhs return') Nothing]
        return' =
            If (InfixApp (Var $ unname "lb")
                         (QVarOp $ UnQual (Symbol "<="))
                         (Var $ unname "s"))
               (App (Var $ unname "return")
                    (Var $ unname "t'"))
               rec'

declRandGen :: [Decl]
declRandGen = declTFun "randomP" type' [] body
    where type' = randGenType "Double"
          body = App (Var $ unname "lift") 
                     (App (Var $ unname "getRandomR") 
                          (Tuple Boxed [Lit $ Int 0, Lit $ Int 1]))

declADTs :: Real a => BoltzmannSystem a -> [Decl]
declADTs sys = map declADT $ paramTypes sys

declADT :: Real a => (String, [Cons a]) -> Decl
declADT (t,cons) = DataDecl noLoc DataType [] (Ident t) []
                            (map (QualConDecl noLoc [] [] . declCon) cons)
                            [(unname "Show", [])]

declCon :: Real a => Cons a -> ConDecl
declCon expr = ConDecl (Ident $ func expr) ags
    where ags = map declArg (args expr)

declArg :: Arg -> Type
declArg (Type s) = TyVar $ Ident s

randomDraw :: String -> Stmt
randomDraw var = Generator noLoc (PVar $ Ident var)
                           (Var $ unname "randomP")

declGenerators :: Real a => BoltzmannSystem a -> [Decl]
declGenerators sys = concatMap declGenerator $ paramTypes' sys

declGenerator :: Real a => (String, [(Cons a, Integer)]) -> [Decl]
declGenerator (t, g @ cons) = declTFun (genName t) type' ["ub"] body
    where type' = generatorType t
          body = constructGenerator g

constructGenerator :: Real a => [(Cons a, Integer)] -> Exp
constructGenerator [con] = returnCons "ub" con
constructGenerator cons = Do [Qualifier $ guardian "ub",
                              randomDraw "p", 
                              Qualifier $ constructCons "p" "ub" cons]

constructCons :: Real a => String -> String -> [(Cons a, Integer)] -> Exp
constructCons p ub [c] = returnCons ub c
constructCons p ub (c:cs) = 
    constructIf p c (returnCons ub c) 
                    (constructCons p ub cs)

constructIf :: Real a => String -> (Cons a, t) -> Exp -> Exp -> Exp
constructIf p (c,_) = If (InfixApp (Var $ unname p)
                                   (QVarOp $ UnQual (Symbol "<"))
                                   (Lit $ Frac (toRational $ weight c)))

nat :: [String]
nat = map show [0..]

variableStream :: [String]
variableStream = map ('x' :) nat

weightVariableStream :: [String]
weightVariableStream = map ('w' :) nat

guardian :: String -> Exp
guardian name = App (Var $ unname "guard") 
                    (InfixApp (Var $ unname name)
                              (QVarOp $ UnQual (Symbol ">"))
                              (Lit $ Int 0))

returnCons :: String -> (Cons a, Integer) -> Exp
returnCons ub (con, w)
    | null (args con) = InfixApp (Var $ unname "return")
                                 (QVarOp $ UnQual $ Symbol "$!")
                                 (Tuple Boxed [Con (unname $ func con), 
                                          Lit (Int w)])
    | otherwise = Do (buildArgs (args con) (InfixApp (Var $ unname ub)
                                                     (QVarOp $ UnQual (Symbol "-"))
                                                     (Lit $ Int w))
                                           (Lit $ Int w)
                                (Con (unname $ func con))
                                variableStream weightVariableStream)

buildArgs [] upperBound currW t vs ws = 
    [Qualifier $ InfixApp (Var $ unname "return")
                          (QVarOp $ UnQual $ Symbol "$!")
                          (Tuple Boxed [t, currW])]

buildArgs (Type a : as) upperBound currW t (v:vs) (w:ws) =
    Generator noLoc (PTuple Boxed [pv', pw'])
                    (App (Var $ unname (genName a)) upperBound)
                    : buildArgs as upperBound' currW' t' vs ws
    where
        t' = App t v'
        (v', w') = (Var $ unname v, Var $ unname w)
        (pv', pw') = (PVar $ Ident v, PVar $ Ident w)
        upperBound' = InfixApp upperBound (QVarOp $ UnQual (Symbol "-")) w'
        currW' = InfixApp currW (QVarOp $ UnQual (Symbol "+")) w'

moduleHeader :: Show a => BoltzmannSystem a -> String -> String
moduleHeader sys compilerNote = unlines ["-- | Compiler: " ++ compilerNote,
                                         "-- | Singularity: " ++ show (parameter sys)]

compile :: (Real a, Show a) => BoltzmannSystem a -> String -> String -> IO ()
compile sys moduleName compilerNote = let module' = compileModule sys moduleName
                                        in do putStr (moduleHeader sys compilerNote)
                                              putStrLn (prettyPrint module')
