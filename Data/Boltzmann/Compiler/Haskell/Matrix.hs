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
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc (noLoc)

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Data.Maybe (fromMaybe)

import Data.Boltzmann.System hiding (List,Cons)
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
    Module noLoc (ModuleName mod') []
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

     importFrom "Control.Monad.Random" ([importType "RandomGen",
                                        importFunc "Rand",
                                        importFunc "getRandomR"]
                                        ++ importIO withIO'),

    importFrom "Data.Vector" [importType "Vector"],
    importQual "Data.Vector" "V"]

importIO :: Bool -> [ImportSpec]
importIO withIO' = [importFunc "evalRandIO" | withIO']

decls :: PSystem Double -> Bool -> [Decl]
decls sys withIO' = [symbolDecl ,stateDecl]
                    ++ declSpecification sys
                    ++ declIsTerminal
                    ++ declIsAbsorbing
                    ++ declRandomP
                    ++ declNextState
                    ++ declGen
                    ++ declSampler
                    ++ declStartingState sys
                    ++ concat [declSamplerIO | withIO']

-- | Type synonym for alphabet letters.
symbolDecl :: Decl
symbolDecl = TypeDecl noLoc (Ident "Symbol") []
    (TyCon $ unname "String")

-- | Graph state data type.
stateDecl :: Decl
stateDecl =
    DataDecl noLoc DataType [] (Ident "State")
        [] conDecl []
    where
        conDecl = [QualConDecl noLoc [] [] (RecDecl (Ident "State") cons)]
        cons = [([Ident "idx"], TyCon $ unname "Int")
               ,([Ident "symb"], TyCon $ unname "Symbol")
               ,([Ident "prob"], TyCon $ unname "Double")
               ,([Ident "weight"], TyCon $ unname "Int")
               ]

-- | Internal state representation.
type State = (Int, String, Double, Int)

-- | Maps a given state to its expression representation.
stateExp :: State -> Exp
stateExp (n, a, p, w) =
    RecConstr (unname "State")
        [FieldUpdate (unname "idx") (toLit n)
        ,FieldUpdate (unname "symb") (toString a)
        ,FieldUpdate (unname "prob") (toDouble p)
        ,FieldUpdate (unname "weight") (toLit w)
        ]

-- | Converts the given system into
--   corresponding a graph representation.
toGraph :: PSystem Double -> [[State]]
toGraph sys = map (typeAdj $ types sys') typs
    where typs = M.toList (defs sys')
          sys' = system sys

          typeAdj typs' (_, cons) = map (consAdj typs') cons

          -- | Assigns the given constructor a reference index,
          --   pointing to the following node (type).
          typeIdx typs' con
              | isAtomic con = -1 -- note: epsilon transition.
              | otherwise = let typ = argName (head $ args con)
                                  in typ `S.findIndex` typs'

          consAdj typs' con = (n, a, p, w)
              where a = func con -- note: that's in fact the transition letter.
                    p = weight con
                    n = typeIdx typs' con
                    w = fromMaybe (length a) $ letterWeight a (alphabet $ system sys)

graphExp :: [[State]] -> Exp
graphExp = List . map (List . map stateExp)

vectorType :: Type
vectorType = typeCons "Vector"

stateType :: Type
stateType  = typeCons "State"

vecExp :: String -> Exp
vecExp s = Var $ Qual (ModuleName "V") (Ident s)

g' :: Type
g' = typeVar "g"

randomGen' :: QName
randomGen' = unname "RandomGen"

rand' :: Type
rand' = typeCons "Rand"

int' :: Type
int' = typeCons "Int"

double' :: Type
double' = typeCons "Double"

maybeT' :: Type
maybeT' = typeCons "MaybeT"

return' :: Exp
return' = varExp "return"

maybeTType :: Type -> Type
maybeTType = TyApp (TyApp maybeT' (TyApp rand' g'))

randomP :: String -> Stmt
randomP v = bind v $ varExp "randomP"

declIsTerminal :: [Decl]
declIsTerminal = declTFun "isTerminal" type' ["s"] body
    where type' = TyForall Nothing [] (TyFun (typeCons "State") (typeCons "Bool"))
          body = App (varExp "idx") (varExp "s") `less` toLit 0

getNeighbours :: Exp
getNeighbours = InfixApp (varExp "specification")
                   (QConOp $ Qual (ModuleName "V") (Symbol "!"))
                   (varExp "s")

declIsAbsorbing :: [Decl]
declIsAbsorbing = declTFun "isAbsorbing" type' ["s"] body
    where type' = TyForall Nothing [] (TyFun int' (typeCons "Bool"))
          body = applyF (varExp "any") [varExp "isTerminal", getNeighbours]

declRandomP :: [Decl]
declRandomP = declTFun "randomP" type' [] body
    where type' = TyForall Nothing [ClassA randomGen' [g']] (maybeTType $ typeVar "Double")
          body = App (varExp "lift")
                     (App (varExp "getRandomR")
                          (Tuple Boxed [toLit 0, toLit 1]))

declSpecification :: PSystem Double -> [Decl]
declSpecification sys = declTFun "specification" type' [] body
    where type' = TyForall Nothing [] (TyApp vectorType (TyList stateType))
          graph = toGraph sys
          body  = App (vecExp "fromList")
                        (graphExp graph)

-- | Choice function determining the next state.
declNextState :: [Decl]
declNextState = [decl, FunBind clauses]
    where decl = TypeSig noLoc [Ident "nextState"] type'
          type' = TyForall Nothing [] (TyFun double'
                        (TyFun (TyList (typeCons "State"))
                        (TyTuple Boxed [int', typeCons "Symbol", int'])))
          clauses = [Match noLoc (Ident "nextState")
                        [PWildCard, PList [PVar (Ident "s")]]
                        Nothing (UnGuardedRhs bodyLess) Nothing,

                     Match noLoc (Ident "nextState")
                        [PVar (Ident "p")
                        ,PInfixApp
                            (PVar (Ident "s"))
                            (Special Cons)
                            (PVar (Ident "xs"))]
                        Nothing
                        (GuardedRhss [guardLess
                                     ,guardOtherwise]) Nothing]

          -- clause for x < p.
          guardLess = GuardedRhs noLoc
                [Qualifier $ varExp "p" `less` App (varExp "prob") (varExp "s")] bodyLess

          bodyLess  = Tuple Boxed [App (varExp "idx") (varExp "s"),
                                   App (varExp "symb") (varExp "s"),
                                   App (varExp "weight") (varExp "s")]

          -- otherwise guard.
          guardOtherwise = GuardedRhs noLoc
                [Qualifier $ varExp "otherwise"] bodyOtherwise

          bodyOtherwise = App (App (varExp "nextState") (varExp "p")) (varExp "xs")

declGen :: [Decl]
declGen = declTFun "genWord" type' ["ub", "s"] mainIfStmt
    where type' = TyForall Nothing [ClassA randomGen' [g']]
            (TyFun int' $ TyFun int' (maybeTType $ TyTuple Boxed
                [TyList $ typeCons "Symbol", int']))

          mainIfStmt = If (varExp "ub" `lessEq` toLit 0
                            `and` applyF (varExp "isAbsorbing") [varExp "s"])
                         (App return' (Tuple Boxed [List [], toLit 0]))
                         mainBody

          mainBody = Do [randomP "p"
                        ,getNext
                        ,ifStmt]

          getNext = LetStmt (BDecls [getNext'])
          getNext' = PatBind noLoc
                        (PTuple Boxed [PVar $ Ident "d", PVar $ Ident "a", PVar $ Ident "n"])
                        (UnGuardedRhs $ App (App (varExp "nextState") (varExp "p"))
                            getNeighbours) Nothing

          ifStmt = Qualifier $ If (less (varExp "d") (toLit 0))
                      (App return' (Tuple Boxed [List [], toLit 0])) elseStmt

          elseStmt = Do [recursiveCall
                        ,returnStmt]

          recursiveCall = Generator noLoc
                            (PTuple Boxed [PVar $ Ident "w", PVar $ Ident "m"])
                            (App (App (varExp "genWord") (varExp "ub" `sub` varExp "n"))
                                (varExp "d"))

          returnStmt = Qualifier $ App return'
                        (Tuple Boxed
                            [InfixApp (varExp "a") (symbol ":") (varExp "w")
                            ,varExp "n" `add` varExp "m"])

declSampler :: [Decl]
declSampler = declTFun "sampleWord" type' ["lb", "ub", "s"] constructSampler
    where type' = TyForall Nothing [ClassA randomGen' [g']]
            (TyFun int' $ TyFun int' $ TyFun int' $ TyApp (TyApp rand' g')
                (TyList $ typeCons "Symbol"))

constructSampler :: Exp
constructSampler =
    Do [bind "sample" (applyF (varExp "runMaybeT")
            [applyF (varExp "genWord") [varExp "ub", varExp "s"]]),
            caseSample]
    where caseSample = Qualifier $ Case (varExp "sample")
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
constructSamplerIO = applyF (varExp "evalRandIO")
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
