{-|
 Module      : Data.Boltzmann.Compiler.Haskell.Algebraic
 Description : Algebraic Boltzmann system compiler for GHC.
 Copyright   : (c) Maciej Bendkowski, 2017-2021

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 Algebraic system compiler using Haskell's built-in algebraic data types
 to handle to given system types. The outcome sampler is a rejection-based
 sampler implementing the anticipated-rejection sampling scheme.
 -}
module Data.Boltzmann.Compiler.Haskell.Algebraic
  ( Conf(..)
  , compile
  , config
  )
where

import           Prelude                 hiding ( and )
import           Language.Haskell.Exts   hiding ( List )
import qualified Language.Haskell.Exts         as LHE

import           Data.Boltzmann.System
import           Data.Boltzmann.Internal.Annotations
import           Data.Boltzmann.Internal.Utils  ( getTime )

import           Data.Boltzmann.Compiler
import           Data.Boltzmann.Compiler.Haskell.Helpers

-- | Default configuration type.
data Conf = Conf { paramSys    :: PSystem Double   -- ^ Parametrised system.
                 , moduleName  :: String           -- ^ Module name.
                 , compileNote :: String           -- ^ Header comment note.
                 , withIO      :: Bool             -- ^ Generate IO actions?
                 , withLists   :: Bool             -- ^ Generate all list samplers?
                 , withShow    :: Bool             -- ^ Generate deriving Show?
                 }

instance Configuration Conf where

  config sys module' compilerNote' =
    let with = withBool (annotations $ system sys)
    in  Conf { paramSys    = sys
             , moduleName  = module'
             , compileNote = compilerNote'
             , withIO      = "withIO" `with` True
             , withShow    = "withShow" `with` True
             , withLists   = "withLists" `with` False
             }

  compile conf =
    let sys        = paramSys conf
        name'      = moduleName conf
        note       = compileNote conf
        withIO'    = withIO conf
        withLists' = withLists conf
        withShow'  = withShow conf
        module'    = compileModule sys name' withIO' withLists' withShow'
    in  do
          time <- getTime
          putStr $ moduleHeader sys note time
          putStrLn $ prettyPrint module'

moduleHeader :: PSystem Double -> String -> String -> String
moduleHeader sys compilerNote time = unlines
  (  [ "-- | Compiler:     " ++ compilerNote
     , "-- | Generated at: " ++ time
     , "-- | Singularity:  " ++ show (param sys)
     ]
  ++ systemNote sys (show Algebraic)
  )

compileModule :: PSystem Double -> String -> Bool -> Bool -> Bool -> Module ()
compileModule sys mod' withIO' withLists' withShow' = Module
  ()
  (Just $ ModuleHead ()
                     (ModuleName () mod')
                     Nothing
                     (Just $ ExportSpecList () exports)
  )
  [ LanguagePragma
      ()
      [ Ident () "TemplateHaskell"
      , Ident () "DeriveGeneric"
      , Ident () "DeriveAnyClass"
      ]
  ]
  imports
  decls
 where
  exports = declareExports sys withIO' withLists'
  imports = declareImports withIO'
  decls =
    declareADTs withShow' sys
      ++ declareDDGs sys
      ++ declareListDecisionTrees sys withLists'
      ++ declareGenerators sys
      ++ declareListGenerators sys withLists'
      ++ declareGenericSampler
      ++ declareSamplersIO sys withIO'
      ++ declareListSamplersIO sys withIO' withLists'

declareImports :: Bool -> [ImportDecl ()]
declareImports withIO' =
  [ importUnQual "GHC.Generics"
  , importUnQual "Data.Aeson"
  , importFrom "Control.Monad"       [importFunc "guard"]
  , importFrom "Control.Monad.Trans" [importFunc "lift"]
  , importFrom "Control.Monad.Trans.Maybe"
               [importType "MaybeT", importFunc "runMaybeT"]
  , importFrom
    "Data.Buffon.Machine"
    (  [ importType' "BuffonMachine"
       , importType "DecisionTree"
       , importFunc "decisionTree"
       , importFunc "choiceDDG"
       ]
    ++ importIO withIO'
    )
  , importFrom "Data.Vector"   [importType "Vector", importFunc "fromList"]
  , importFrom "System.Random" [importType "RandomGen"]
  ]

importIO :: Bool -> [ImportSpec ()]
importIO withIO' = [ importFunc "runRIO" | withIO' ]

-- Naming functions.
genName :: ShowS
genName = (++) "genRandom"

ddgName :: ShowS
ddgName = (++) "ddg"

decisionTreeListName :: ShowS
decisionTreeListName = (++) "decisionTreeList"

listGenName :: ShowS
listGenName t = genName t ++ "List"

samplerName :: ShowS
samplerName = (++) "sample"

listSamplerName :: ShowS
listSamplerName t = samplerName t ++ "List"

samplerIOName :: ShowS
samplerIOName t = samplerName t ++ "IO"

listSamplerIOName :: ShowS
listSamplerIOName t = listSamplerName t ++ "IO"

genericSamplerName :: String
genericSamplerName = "sample"

declareExports :: PSystem Double -> Bool -> Bool -> [ExportSpec ()]
declareExports sys withIO' withLists' =
  exportTypes sys
    ++ exportGenerators sys
    ++ exportListGenerators sys withLists'
    ++ exportGenericSampler
    ++ exportSamplersIO sys withIO'
    ++ exportListSamplersIO sys withIO' withLists'

exportGenerators :: PSystem Double -> [ExportSpec ()]
exportGenerators sys = map (exportFunc . genName) $ typeList sys

exportListGenerators :: PSystem Double -> Bool -> [ExportSpec ()]
exportListGenerators sys withLists' = map (exportFunc . listGenName)
  $ types' sys
  where types' = if withLists' then typeList else seqTypes . system

exportGenericSampler :: [ExportSpec ()]
exportGenericSampler = [exportFunc genericSamplerName]

exportSamplersIO :: PSystem Double -> Bool -> [ExportSpec ()]
exportSamplersIO _   False = []
exportSamplersIO sys True  = map (exportFunc . samplerIOName) $ typeList sys

exportListSamplersIO :: PSystem Double -> Bool -> Bool -> [ExportSpec ()]
exportListSamplersIO _   False _          = []
exportListSamplersIO sys True  withLists' = map (exportFunc . listSamplerIOName)
  $ types' sys
  where types' = if withLists' then typeList else seqTypes . system

-- Generators.
maybeTType :: Type () -> Type ()
maybeTType = TyApp () (TyApp () maybeT' (TyApp () buffonMachine' g'))

generatorType :: Type () -> Type ()
generatorType type' = TyForall
  ()
  Nothing
  (Just $ CxTuple () [ClassA () randomGen' [g']])
  (TyFun () int' (maybeTType $ TyTuple () Boxed [type', int']))

guardian :: String -> Stmt ()
guardian v =
  Qualifier () $ App () (varExp "guard") (varExp v `greater` toLit 0)

declareDDGs :: PSystem Double -> [Decl ()]
declareDDGs sys = concatMap declareDDG (paramTypesDDG sys)

declareDDG :: (String, [Int]) -> [Decl ()]
declareDDG (t, ddg) = declTFun name' type' [] body
 where
  type' = ddgType
  name' = ddgName t
  body  = spliceExp quote
  quote = QuasiQuote () "" (prettyPrint dt')
  dt'   = applyF (varExp "fromList") [ddg']
  ddg'  = LHE.List () (intList ddg)

declareDecisionT :: Exp () -> String -> [Decl ()]
declareDecisionT prob name' = declTFun name' type' [] body
 where
  type' = decisionTreeType
  body  = spliceExp lift'
  lift' = applyF (qVarExp "TH" "lift") [dt']
  dt'   = applyF (varExp "decisionTree") [prob]

declareGenerators :: PSystem Double -> [Decl ()]
declareGenerators sys = concatMap declGenerator (paramTypesW sys)

declGenerator :: (String, [(Cons Double, Int)]) -> [Decl ()]
declGenerator (t, g) = declTFun (genName t) type' ["ub"] body
 where
  type' = generatorType $ typeCons t
  body  = constrGenerator t g

constrGenerator :: String -> [(Cons Double, Int)] -> Exp ()
constrGenerator _ [(constr, w)] = rec True constr w
constrGenerator t cs            = Do () (initSteps ++ branching)
 where
  branching = [Qualifier () $ Case () (varExp "n") (constrGenerator' 0 cs)]
  initSteps = [guardian "ub", choiceN "n" (varExp $ ddgName t)]

constrGenerator' :: Int -> [(Cons Double, Int)] -> [Alt ()]
constrGenerator' _ [(constr, w)] =
  [caseAlt' (UnGuardedRhs () $ rec False constr w)]

constrGenerator' n ((constr, w) : cs) =
  caseAlt (show n) (UnGuardedRhs () $ rec False constr w)
    : constrGenerator' (succ n) cs

constrGenerator' _ _ = error "I wasn't expecting the Spanish inquisition!"

rec :: Bool -> Cons Double -> Int -> Exp ()
rec withGuardian constr w =
  case arguments (args constr) (toLit w) variableStream weightStream of
    ([]   , _     , _ ) -> applyF return' [Tuple () Boxed [cons, toLit w]]
    (stmts, totalW, xs) -> Do
      ()
      (  [ guardian "ub" | withGuardian ]
      ++ stmts
      ++ [ret cons xs (toLit w `add` totalW)]
      )
  where cons = conExp $ func constr

arguments
  :: [Arg] -> Exp () -> [String] -> [String] -> ([Stmt ()], Exp (), [Exp ()])
arguments []                 _  _  _  = ([], toLit 0, [])
arguments (Type arg : args') ub xs ws = arguments' genName arg args' ub xs ws
arguments (List arg : args') ub xs ws =
  arguments' listGenName arg args' ub xs ws

arguments'
  :: (t -> String)
  -> t
  -> [Arg]
  -> Exp ()
  -> [String]
  -> [String]
  -> ([Stmt ()], Exp (), [Exp ()])
arguments' f arg args' ub (x : xs) (w : ws) = (stmt : stmts, argW', v : vs)
 where
  stmt              = bindP x w $ applyF (varExp $ f arg) [varExp "ub" `sub` ub]
  (stmts, argW, vs) = arguments args' ub' xs ws
  argW'             = argW `add` varExp w
  ub'               = ub `sub` varExp w
  v                 = varExp x

arguments' _ _ _ _ _ _ = error "I wasn't expecting the Spanish inquisition!"

ret :: Exp () -> [Exp ()] -> Exp () -> Stmt ()
ret f [] w = Qualifier () $ applyF return' [Tuple () Boxed [f, w]]
ret f xs w = Qualifier () $ applyF return' [Tuple () Boxed [t, w]]
  where t = applyF f xs

-- List generators.
declareListDecisionTrees :: PSystem Double -> Bool -> [Decl ()]
declareListDecisionTrees sys withLists' = concatMap
  (declareListDecisionTree sys)
  (types' sys)
  where types' = if withLists' then typeList else seqTypes . system

declareListDecisionTree :: PSystem Double -> String -> [Decl ()]
declareListDecisionTree sys t = declareDecisionT prob name'
 where
  name' = decisionTreeListName t
  prob  = LHE.List () [Lit () (Frac () t' (show t'))]
  t'    = toRational $ typeWeight sys t

listGeneratorType :: Type () -> Type ()
listGeneratorType type' = TyForall
  ()
  Nothing
  (Just $ CxTuple () [ClassA () randomGen' [g']])
  (TyFun () int' (maybeTType $ TyTuple () Boxed [TyList () type', int']))

declareListGenerators :: PSystem Double -> Bool -> [Decl ()]
declareListGenerators sys withLists' = concatMap declListGenerator $ types' sys
  where types' = if withLists' then typeList else seqTypes . system

declListGenerator :: String -> [Decl ()]
declListGenerator t = declTFun (listGenName t) type' ["ub"] body
 where
  type' = listGeneratorType (typeCons t)
  body  = constrListGenerator t

constrListGenerator :: String -> Exp ()
constrListGenerator t = Do () (initSteps ++ branching)
 where
  branching = [Qualifier () $ Case () (varExp "n") (constrListGenerator' 0 t)]
  initSteps = [guardian "ub", choiceN "n" (varExp $ ddgName t)]

constrListGenerator' :: Int -> String -> [Alt ()]
constrListGenerator' n t =
  [ caseAlt (show n) (UnGuardedRhs () $ retHeadList t)
  , caseAlt' (UnGuardedRhs () retNil)
  ]

retHeadList :: String -> Exp ()
retHeadList t = Do
  ()
  [ bindP "x" "w" (applyF (varExp $ genName t) [varExp "ub"])
  , bindP "xs"
          "ws"
          (applyF (varExp $ listGenName t) [varExp "ub" `sub` varExp "w"])
  , ret (InfixApp () (varExp "x") (symbol ":") (varExp "xs"))
        []
        (varExp "w" `add` varExp "ws")
  ]

retNil :: Exp ()
retNil = applyF return' [Tuple () Boxed [LHE.List () [], toLit 0]]

-- Samplers.
genericSamplerType :: Type ()
genericSamplerType = TyForall
  ()
  Nothing
  (Just $ CxTuple () [ClassA () randomGen' [g']])
  (TyFun
    ()
    (TyFun ()
           int'
           (maybeTType $ TyTuple () Boxed [TyVar () (Ident () "a"), int'])
    )
    (TyFun
      ()
      int'
      (TyFun ()
             int'
             (TyApp () (TyApp () buffonMachine' g') $ TyVar () (Ident () "a"))
      )
    )
  )

declareGenericSampler :: [Decl ()]
declareGenericSampler = declTFun genericSamplerName
                                 type'
                                 ["gen", "lb", "ub"]
                                 body
 where
  type' = genericSamplerType
  body  = constructGenericSampler

constructGenericSampler :: Exp ()
constructGenericSampler = Do
  ()
  [ bind "str"
         (applyF (varExp "runMaybeT") [applyF (varExp "gen") [varExp "ub"]])
  , caseSample
  ]
 where
  caseSample = Qualifier () $ Case
    ()
    (varExp "str")
    [ Alt () (PApp () (unname "Nothing") []) (UnGuardedRhs () rec') Nothing
    , Alt
      ()
      (PApp ()
            (unname "Just")
            [PTuple () Boxed [PVar () $ Ident () "x", PVar () $ Ident () "s"]]
      )
      (UnGuardedRhs () return'')
      Nothing
    ]

  rec' = applyF (varExp genericSamplerName)
                [varExp "gen", varExp "lb", varExp "ub"]
  return'' = If
    ()
    (lessEq (varExp "lb") (varExp "s") `and` lessEq (varExp "s") (varExp "ub"))
    (applyF (varExp "return") [varExp "x"])
    rec'

-- IO Samplers.
samplerIOType :: Type () -> Type ()
samplerIOType type' = TyForall
  ()
  Nothing
  Nothing
  (TyFun () int' (TyFun () int' (TyApp () (typeVar "IO") type')))

declareSamplersIO :: PSystem Double -> Bool -> [Decl ()]
declareSamplersIO _   False = []
declareSamplersIO sys True  = concatMap declSamplerIO $ typeList sys

declSamplerIO :: String -> [Decl ()]
declSamplerIO t = declTFun (samplerIOName t) type' ["lb", "ub"] body
 where
  type' = samplerIOType (typeCons t)
  body  = constructSamplerIO genName t

constructSamplerIO :: (a -> String) -> a -> Exp ()
constructSamplerIO f t = applyF
  (varExp "runRIO")
  [applyF (varExp genericSamplerName) [varExp (f t), varExp "lb", varExp "ub"]]

declareListSamplersIO :: PSystem Double -> Bool -> Bool -> [Decl ()]
declareListSamplersIO _   False _          = []
declareListSamplersIO sys True  withLists' = concatMap declListSamplerIO
  $ types' sys
  where types' = if withLists' then typeList else seqTypes . system

declListSamplerIO :: String -> [Decl ()]
declListSamplerIO t = declTFun (listSamplerIOName t) type' ["lb", "ub"] body
 where
  type' = samplerIOType (TyList () $ typeCons t)
  body  = constructSamplerIO listGenName t
