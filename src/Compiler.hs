-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Compiler
    ( compile 
    ) where

import Language.Haskell.Exts
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (noLoc)

import System
import BoltzmannSystem

import Oracle

unname :: String -> QName
unname = UnQual . Ident

genName :: String -> String
genName = (++) "genRandom"

samplerName :: String -> String
samplerName = (++) "sample"

declTFun f type' body = [decl, FunBind [main]]
    where decl = TypeSig noLoc [Ident f] type'
          main = Match noLoc (Ident f) [] Nothing
                       (UnGuardedRhs body) Nothing

declPatternFun f cases = FunBind body
    where body = map match' cases
          match' (pat,b) = Match noLoc (Ident f) pat Nothing
                           (UnGuardedRhs b) Nothing

generatorType typeName = TyForall Nothing 
    [ClassA (unname "RandomGen") [TyVar $ Ident "g"]]
    (TyApp (TyApp (TyCon (unname "Rand")) (TyVar (Ident "g")))
           (TyCon (unname typeName)))

samplerType typeName = TyForall Nothing 
    [ClassA (unname "RandomGen") [TyVar $ Ident "g"]]
    (TyFun (TyCon $ unname "Int") 
           (TyFun (TyCon $ unname "Int") 
                  (TyApp (TyApp (TyCon (unname "Rand")) (TyVar (Ident "g")))
                  (TyCon (unname typeName)))))

compileModule sys moduleName = Module noLoc (ModuleName moduleName) []
                                      Nothing (Just exports) imports decls
    where
        exports = compileExports sys
        imports = compileImports sys
        decls = compileDecls sys

compileExports sys = concatMap toADT $ typeList sys
    where toADT t = [EThingAll (unname t),
                     EVar (unname $ genName t),
                     EVar (unname $ samplerName t)]

compileImports _ = [ImportDecl { importLoc = noLoc
                               , importModule = ModuleName "Control.Monad.Random"
                               , importQualified = False
                               , importSrc = False
                               , importSafe = False
                               , importPkg = Nothing
                               , importAs = Nothing
                               , importSpecs = Nothing
                               }]

compileDecls sys = declCombClass : declADTs sys ++ declCombInstances sys 
                 ++ declRandGen ++ declGenerators sys ++ declSamplers sys

declCombClass = ClassDecl noLoc [] (Ident "Combinatorial") [UnkindedVar $ Ident "a"]
                          [] [ClsDecl $ TypeSig noLoc [Ident "size"]
                           (TyFun (TyVar $ Ident "a") (TyCon $ unname "Int"))]

declCombInstances sys = map declCombInst $ weightedTypes sys

declCombInst (t,cons) = InstDecl noLoc Nothing [] [] (unname "Combinatorial")
                                 [TyCon $ unname t] [InsDecl sizeDecl]
    where sizeDecl = declPatternFun "size" cases
          cases = map constructConSize cons

constructConSize con = ([pat], expr)
    where pat = PApp (unname $ func con) argPatterns
          argPatterns = map (PVar . Ident . snd) vars
          vars = zip (args con) variableStream
          sizeF (arg,v) = App (Var $ unname "size") (Var $ unname v)
          expr = foldl (\f g -> InfixApp f (QVarOp $ UnQual (Symbol "+")) g)
                       (Lit (Int $ weight con)) (map sizeF vars)

declSamplers sys = concatMap declSampler $ typeList sys

declSampler t = declTFun (samplerName t) type' body
    where type' = samplerType t 
          body = constructSampler t

constructSampler t = Lambda noLoc [PVar (Ident "lb"), PVar (Ident "ub")] 
                        $ Do [draw, bindsize, check]
    where draw = Generator noLoc (PVar $ Ident "x") 
                                 (Var $ unname (genName t))
          bindsize = LetStmt (BDecls [PatBind noLoc (PVar $ Ident "s") rhs Nothing])
          rhs = UnGuardedRhs $ App (Var $ unname "size") (Var $ unname "x")
          check = Qualifier $ If (InfixApp lbcheck (QVarOp $ UnQual (Symbol "||")) ubcheck)
                     (App (App (Var $ unname (samplerName t)) 
                         (Var $ unname "lb")) (Var $ unname "ub"))
                     (App (Var $ unname "return") (Var $ unname "x"))
          
          lbcheck = InfixApp (Var $ unname "s") 
                             (QVarOp $ UnQual (Symbol "<")) 
                             (Var $ unname "lb")
          
          ubcheck = InfixApp (Var $ unname "ub") 
                             (QVarOp $ UnQual (Symbol "<")) 
                             (Var $ unname "s")

declRandGen = declTFun "randomP" type' body
    where type' = generatorType "Double"
          body = App (Var $ unname "getRandomR") 
                     (Tuple Boxed [Lit $ Int 0, Lit $ Int 1])

declADTs :: BoltzmannSystem -> [Decl]
declADTs sys = map declADT $ paramTypes sys

declADT (t,cons) = DataDecl noLoc DataType [] (Ident t) []
                            (map (QualConDecl noLoc [] [] . declCon) cons)
                            [(unname "Show", [])]

declCon expr = ConDecl (Ident $ func expr) ags
    where ags = map declArg (args expr)

declArg (Type s) = TyVar $ Ident s

randomDraw var = Generator noLoc (PVar $ Ident var)
                           (Var $ unname "randomP")

declGenerators :: BoltzmannSystem -> [Decl]
declGenerators sys = concatMap declGenerator $ paramTypes sys

declGenerator g @ (t,cons) = declTFun (genName t) type' body
    where type' = generatorType t
          body = constructGenerator g

constructGenerator (t, cons) = Do [randomDraw "p", 
                                   Qualifier $ constructCons "p" cons]

constructCons p [c] = returnCons c
constructCons p (c:cs) = constructIf p c (returnCons c) (constructCons p cs)

constructIf p c = If (InfixApp (Var $ unname p)
                               (QVarOp $ UnQual (Symbol "<"))
                               (Lit $ Frac (toRational $ weight c)))

variableStream = map ('x' :) nat
    where nat = map show [0..]

returnCons con = Do (binds ++ [Qualifier return'])
    where return' = App (Var $ unname "return") result
          conargs = zip (args con) variableStream
          binds = map callGenerator conargs
          vars = map (Var . unname . snd) conargs
          result = foldl App (Con $ unname (func con)) vars

callGenerator (Type t, v) = Generator noLoc (PVar $ Ident v) call
    where call = Var $ unname (genName t)

compile :: BoltzmannSystem -> String -> IO ()
compile sys moduleName = putStrLn (prettyPrint module')
    where module' = compileModule sys moduleName
