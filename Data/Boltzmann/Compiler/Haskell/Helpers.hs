{-|
 Module      : Data.Boltzmann.Compiler.Haskell.Helpers
 Description : Helper methods for GHC syntax.
 Copyright   : (c) Maciej Bendkowski, 2017-2020

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 General utilities used across system compilers.
 -}
module Data.Boltzmann.Compiler.Haskell.Helpers where

import           Language.Haskell.Exts   hiding ( List )

import           Data.Boltzmann.System

-- | Prepares a system/module note.
systemNote :: PSystem Double -> String -> [String]
systemNote psys t =
  [ "-- | System type:  " ++ t
  , "-- | Target size:  " ++ show (structureSize sys)
  , "-- | Stability:    experimental"
  ]
  where sys = system psys

unname :: String -> QName ()
unname = UnQual () . Ident ()

typeCons :: String -> Type ()
typeCons = TyCon () . unname

typeVar :: String -> Type ()
typeVar = TyVar () . Ident ()

varExp :: String -> Exp ()
varExp = Var () . unname

qVarExp :: String -> String -> Exp ()
qVarExp m s = Var () $ Qual () (ModuleName () m) (Ident () s)

conExp :: String -> Exp ()
conExp = Con () . unname

spliceExp :: Exp () -> Exp ()
spliceExp = SpliceExp () . ParenSplice ()

toLit :: Int -> Exp ()
toLit n = Lit () (Int () (toInteger n) (show n))

importType :: String -> ImportSpec ()
importType = IThingAll () . Ident ()

importType' :: String -> ImportSpec ()
importType' = IAbs () (NoNamespace ()) . Ident ()

importFunc :: String -> ImportSpec ()
importFunc = IVar () . Ident ()

-- | Simple import declaration.
importFrom :: String -> [ImportSpec ()] -> ImportDecl ()
importFrom module' specs = ImportDecl
  { importAnn       = ()
  , importModule    = ModuleName () module'
  , importQualified = False
  , importSrc       = False
  , importSafe      = False
  , importPkg       = Nothing
  , importAs        = Nothing
  , importSpecs     = Just $ ImportSpecList () False specs
  }

importQual :: String -> String -> ImportDecl ()
importQual module' synonym = ImportDecl
  { importAnn       = ()
  , importModule    = ModuleName () module'
  , importQualified = True
  , importSrc       = False
  , importSafe      = False
  , importPkg       = Nothing
  , importAs        = Just (ModuleName () synonym)
  , importSpecs     = Nothing
  }

importUnQual :: String -> ImportDecl ()
importUnQual module' = ImportDecl { importAnn       = ()
                                  , importModule    = ModuleName () module'
                                  , importQualified = False
                                  , importSrc       = False
                                  , importSafe      = False
                                  , importPkg       = Nothing
                                  , importAs        = Nothing
                                  , importSpecs     = Nothing
                                  }

exportType :: String -> ExportSpec ()
exportType s = EThingWith () (NoWildcard ()) (unname s) []

exportTypes :: PSystem Double -> [ExportSpec ()]
exportTypes sys = map exportType $ typeList sys

exportFunc :: String -> ExportSpec ()
exportFunc = EVar () . unname

-- | Simple function declaration.
declTFun :: String -> Type () -> [String] -> Exp () -> [Decl ()]
declTFun f type' args' body = [decl, FunBind () [main]]
 where
  decl   = TypeSig () [Ident () f] type'
  args'' = map (PVar () . Ident ()) args'
  main   = Match () (Ident () f) args'' (UnGuardedRhs () body) Nothing

symbol :: String -> QOp ()
symbol s = QVarOp () $ UnQual () (Symbol () s)

greater :: Exp () -> Exp () -> Exp ()
greater x = InfixApp () x (symbol ">")

less :: Exp () -> Exp () -> Exp ()
less x = InfixApp () x (symbol "<")

and :: Exp () -> Exp () -> Exp ()
and x = InfixApp () x (symbol "&&")

lessEq :: Exp () -> Exp () -> Exp ()
lessEq x = InfixApp () x (symbol "<=")

toDouble :: Double -> Exp ()
toDouble x = Lit () $ Frac () (toRational x) (show (toRational x))

toString :: String -> Exp ()
toString s = Lit () $ String () s s

lessF :: Real a => Exp () -> a -> Exp ()
lessF v x = less v (Lit () (Frac () (toRational x) (show (toRational x))))

bind :: String -> Exp () -> Stmt ()
bind v = Generator () (PVar () $ Ident () v)

bindP :: String -> String -> Exp () -> Stmt ()
bindP x y =
  Generator () (PTuple () Boxed [PVar () (Ident () x), PVar () (Ident () y)])

sub :: Exp () -> Exp () -> Exp ()
sub x (Lit () (Int () 0 "0")) = x
sub x y                       = InfixApp () x (symbol "-") y

add :: Exp () -> Exp () -> Exp ()
add x (Lit () (Int () 0 "0")) = x
add (Lit () (Int () 0 "0")) x = x
add x y = InfixApp () x (symbol "+") y

applyF :: Exp () -> [Exp ()] -> Exp ()
applyF = foldl (App ())

dot :: Exp () -> Exp () -> Exp ()
dot x = InfixApp () x (symbol ".")

declareADTs :: Bool -> PSystem a -> [Decl ()]
declareADTs withShow sys = map (declADT withShow) $ paramTypes sys

declADT :: Bool -> (String, [Cons a]) -> Decl ()
declADT withShow (t, [con]) = DataDecl
  ()
  flag
  Nothing
  (DHead () (Ident () t))
  [QualConDecl () Nothing Nothing (declCon con)]
  (declDerivations withShow)
  where
  -- generate a newtype or data type?
        flag = if length (args con) == 1 then NewType () else DataType ()

declADT withShow (t, cons) = DataDecl
  ()
  (DataType ())
  Nothing
  (DHead () (Ident () t))
  (map (QualConDecl () Nothing Nothing . declCon) cons)
  (declDerivations withShow)

declDerivations :: Bool -> [Deriving ()]
declDerivations withShow = [Deriving () Nothing rules]
 where
  rules        = derivGeneric : derivAeson ++ [ derivShow | withShow ]
  derivGeneric = IRule () Nothing Nothing (IHCon () (unname "Generic"))
  derivAeson =
    [ IRule () Nothing Nothing (IHCon () (unname "ToJSON"))
    , IRule () Nothing Nothing (IHCon () (unname "FromJSON"))
    ]
  derivShow = IRule () Nothing Nothing (IHCon () (unname "Show"))


declCon :: Cons a -> ConDecl ()
declCon expr = ConDecl () (Ident () $ func expr) ags
  where ags = map declArg (args expr)

declArg :: Arg -> Type ()
declArg (Type s) = typeVar s
declArg (List s) = TyList () $ typeVar s

caseAlt :: String -> Rhs () -> Alt ()
caseAlt n rhs = Alt () (PVar () $ Ident () n) rhs Nothing

caseAlt' :: Rhs () -> Alt ()
caseAlt' rhs = Alt () (PWildCard ()) rhs Nothing

caseInt :: String -> [(Int, Rhs ())] -> Exp ()
caseInt n xs = Case () (varExp n) (caseInt' xs)

caseInt' :: [(Int, Rhs ())] -> [Alt ()]
caseInt' []              = error "Absurd case"
caseInt' [(_, rhs)     ] = [caseAlt' rhs]
caseInt' ((n, rhs) : xs) = x : caseInt' xs where x = caseAlt (show n) rhs

-- Utils.
maybeT' :: Type ()
maybeT' = typeCons "MaybeT"

buffonMachine' :: Type ()
buffonMachine' = typeCons "BuffonMachine"

int' :: Type ()
int' = typeCons "Int"

g' :: Type ()
g' = typeVar "g"

randomGen' :: QName ()
randomGen' = unname "RandomGen"

return' :: Exp ()
return' = varExp "return"

double' :: Type ()
double' = typeCons "Double"

nat :: [String]
nat = map show ([0 ..] :: [Integer])

variableStream :: [String]
variableStream = map ('x' :) nat

weightStream :: [String]
weightStream = map ('w' :) nat

decisionTreeType :: Type ()
decisionTreeType =
  TyForall () Nothing Nothing (TyApp () (typeCons "DecisionTree") int')

probList :: [(Cons Double, Int)] -> [Exp ()]
probList = map (\x -> Lit () (Frac () (f x) (show (f x))))
  where f = toRational . weight . fst

choiceN :: String -> Exp () -> Stmt ()
choiceN v s = bind v $ applyF (varExp "lift") [applyF (varExp "choice") [s]]
