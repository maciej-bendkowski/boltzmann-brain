{-|
 Module      : Data.Boltzmann.Compiler.Haskell.Helpers
 Description : Helper methods for ghc-7.10.3 compiler syntax.
 Copyright   : (c) Maciej Bendkowski, 2017-2019

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 General utilities used across system compilers.
 -}
module Data.Boltzmann.Compiler.Haskell.Helpers where

import Language.Haskell.Exts hiding (List)
import Language.Haskell.Exts.SrcLoc (noLoc)

import Data.Boltzmann.System

-- | Prepares a system/module note.
systemNote :: PSystem Double -> [String]
systemNote psys = ["-- | System size: " ++ show n,
                   "-- | Constructors: " ++ show m]
    where sys = system psys
          m   = constructors sys
          n   = size sys

unname :: String -> QName
unname = UnQual . Ident

typeCons :: String -> Type
typeCons = TyCon . unname

typeVar :: String -> Type
typeVar = TyVar . Ident

varExp :: String -> Exp
varExp = Var . unname

conExp :: String -> Exp
conExp = Con . unname

toLit :: Int -> Exp
toLit = Lit . Int . toInteger

importType :: String -> ImportSpec
importType = IThingAll . Ident

importFunc :: String -> ImportSpec
importFunc = IVar . Ident

-- | Simple import declaration.
importFrom :: String -> [ImportSpec] -> ImportDecl
importFrom module' specs =
        ImportDecl { importLoc       = noLoc
                   , importModule    = ModuleName module'
                   , importQualified = False
                   , importSrc       = False
                   , importSafe      = False
                   , importPkg       = Nothing
                   , importAs        = Nothing
                   , importSpecs     = Just (False, specs)
                   }

importQual :: String -> String -> ImportDecl
importQual module' synonym =
        ImportDecl { importLoc       = noLoc
                   , importModule    = ModuleName module'
                   , importQualified = True
                   , importSrc       = False
                   , importSafe      = False
                   , importPkg       = Nothing
                   , importAs        = Just (ModuleName synonym)
                   , importSpecs     = Nothing
                   }

exportType :: String -> ExportSpec
exportType = EThingAll . unname

exportTypes :: PSystem Double -> [ExportSpec]
exportTypes sys = map exportType $ typeList sys

exportFunc :: String -> ExportSpec
exportFunc = EVar . unname

-- | Simple function declaration.
declTFun :: String -> Type -> [String] -> Exp -> [Decl]
declTFun f type' args' body = [decl, FunBind [main]]
    where decl   = TypeSig noLoc [Ident f] type'
          args'' = map (PVar . Ident) args'
          main   = Match noLoc (Ident f) args'' Nothing
                         (UnGuardedRhs body) Nothing

symbol :: String -> QOp
symbol s = QVarOp $ UnQual (Symbol s)

greater :: Exp -> Exp -> Exp
greater x = InfixApp x (symbol ">")

less :: Exp -> Exp -> Exp
less x = InfixApp x (symbol "<")

and :: Exp -> Exp -> Exp
and x = InfixApp x (symbol "&&")

lessEq :: Exp -> Exp -> Exp
lessEq x = InfixApp x (symbol "<=")

toDouble :: Double -> Exp
toDouble = Lit . Frac . toRational

toString :: String -> Exp
toString = Lit . String

lessF :: Real a => Exp -> a -> Exp
lessF v x = less v (Lit $ Frac (toRational x))

bind :: String -> Exp -> Stmt
bind v = Generator noLoc (PVar $ Ident v)

bindP :: String -> String -> Exp -> Stmt
bindP x y = Generator noLoc (PTuple Boxed [PVar (Ident x), PVar (Ident y)])

sub :: Exp -> Exp -> Exp
sub x (Lit (Int 0)) = x
sub x y             = InfixApp x (symbol "-") y

add :: Exp -> Exp -> Exp
add x (Lit (Int 0)) = x
add (Lit (Int 0)) x = x
add x y             = InfixApp x (symbol "+") y

applyF :: Exp -> [Exp] -> Exp
applyF = foldl App

dot :: Exp -> Exp -> Exp
dot x = InfixApp x (symbol ".")

declareADTs :: Bool -> PSystem a -> [Decl]
declareADTs withShow sys = map (declADT withShow) $ paramTypes sys

declADT :: Bool -> (String, [Cons a]) -> Decl
declADT withShow (t,[con]) = DataDecl noLoc flag [] (Ident t) []
                               [QualConDecl noLoc [] [] (declCon con)]
                               [(unname "Show", []) | withShow]

    -- generate a newtype or data type?
   where flag = if length (args con) == 1 then NewType
                                          else DataType

declADT withShow (t,cons) = DataDecl noLoc DataType [] (Ident t) []
                              (map (QualConDecl noLoc [] [] . declCon) cons)
                              [(unname "Show", []) | withShow]

declCon :: Cons a -> ConDecl
declCon expr = ConDecl (Ident $ func expr) ags
    where ags = map declArg (args expr)

declArg :: Arg -> Type
declArg (Type s) = typeVar s
declArg (List s) = TyList $ typeVar s
