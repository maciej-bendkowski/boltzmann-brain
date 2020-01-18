{-|
 Module      : Data.Boltzmann.System.Renderer
 Description : Simple graph rendering utilities for combinatorial structures.
 Copyright   : (c) Maciej Bendkowski, 2017-2020

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 -}
module Data.Boltzmann.System.Renderer
    ( ColorScheme(..)
    , toDotFile
    ) where

import Control.Monad

import Data.GraphViz
import Data.GraphViz.Printing
import Data.GraphViz.Attributes.Colors hiding (ColorScheme)
import Data.GraphViz.Attributes.Complete hiding (ColorScheme)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import System.Random
import Data.Text.Lazy (Text)

import Data.Boltzmann.System.Sampler

-- | Ranked trees.
data RTree = RNode { children :: [RTree]
                   , colorT   :: Color
                   , label    :: Int
                   }

-- | Color scheme for nodes.
data ColorScheme = RandomCol  -- ^ Nodes get random colors beased on their type.
                 | BlackCol   -- ^ Each node get the same, black color.

-- | Constructrs an appripriate color attribute.
toColorAttr :: RTree -> Attribute
toColorAttr t = FillColor $ toColorList [colorT t]

-- | Renders the tree nodes.
renderNodes :: RTree -> [DotNode String]
renderNodes t = DotNode (show $ label t) [toColorAttr t] : ts
    where ts = concatMap renderNodes (children t)

-- | Renders tree edges.
renderEdges :: RTree -> [DotEdge String]
renderEdges t = egs ++ concatMap renderEdges (children t)
    where egs = map (\x -> DotEdge (show $ label t) (show $ label x) []) (children t)

-- | Converts a given structure into its ranked variant
--    where each node type is assigned a random RGB color.
toRTree :: ColorScheme
        -> Structure
        -> IO RTree

toRTree RandomCol s = do
    (t, _, _) <- toRTree' M.empty 0 s
    return t

toRTree BlackCol s = return $ fst (toBlackRTree' 0 s)

-- | Node color map.
type ColorMap = Map String Color

toRTree' :: ColorMap
         -> Int -> Structure
         -> IO (RTree, Int, ColorMap)

toRTree' cm rk str =
    let s = name str in
    case s `M.lookup` cm of
        Nothing -> do
            col             <- getColor
            let cm'         = M.insert s col cm
            (xs, rk', cm'') <- toRTreeL' cm' rk (nodes str)
            return (RNode { children = xs
                          , colorT   = col
                          , label    = rk'
                          }, rk' + 1, cm'')
        Just col -> do
            (xs, rk', cm') <- toRTreeL' cm rk (nodes str)
            return (RNode { children = xs
                          , colorT   = col
                          , label    = rk'
                          }, rk' + 1, cm')

toRTreeL' :: ColorMap
          -> Int -> [Structure]
          -> IO ([RTree], Int, ColorMap)

toRTreeL' cm rk [] = return ([], rk, cm)
toRTreeL' cm rk (x:xs) = do
    (x', rk', cm')    <- toRTree' cm rk x
    (xs', rk'', cm'') <- toRTreeL' cm' rk' xs
    return (x' : xs', rk'', cm'')

toBlackRTree' :: Int -> Structure
              -> (RTree, Int)

toBlackRTree' rk str =
    let (xs, rk') = toBlackRTreeL' rk (nodes str)
     in (RNode { children = xs
               , colorT   = black
               , label    = rk'
               }, rk' + 1)

toBlackRTreeL' :: Int -> [Structure]
               -> ([RTree], Int)

toBlackRTreeL' rk [] = ([], rk)
toBlackRTreeL' rk (x:xs) =
    let (x', rk')   = toBlackRTree' rk x
        (xs', rk'') = toBlackRTreeL' rk' xs
        in (x' : xs', rk'')

black :: Color
black = RGB { red   = 0
            , green = 0
            , blue  = 0
            }

-- | Constructs a random rgb color.
getColor :: IO Color
getColor = do
    (r, g, b) <- liftM3 (,,) randomIO randomIO randomIO
    return RGB { red = r, green = g, blue = b }

-- | Given a tree structure, produces a suitable dotfile  representation.
toGraph :: ColorScheme
        -> Structure
        -> IO (DotGraph String)

toGraph cs t = do
    rT         <- toRTree cs t
    let nodes' = renderNodes rT
    let edges' = renderEdges rT
    return DotGraph { strictGraph     = False
                    , directedGraph   = False
                    , graphID         = Nothing
                    , graphStatements = DotStmts {
                        attrStmts = [NodeAttrs [style filled
                                               ,color Black
                                               ,shape Circle
                                               ,toLabel ""
                                               ,Width 0.4]]
                        , subGraphs = []
                        , nodeStmts = nodes'
                        , edgeStmts = edges'
                      }
                 }

-- | Prints a dotfile representation of the given structure.
toDotFile :: ColorScheme
          -> Structure
          -> IO Text

toDotFile cs s = do
    g <- toGraph cs s
    return $ renderDot (toDot g)
