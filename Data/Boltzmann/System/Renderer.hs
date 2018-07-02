{-|
 Module      : Data.Boltzmann.System.Renderer
 Description : Simple graph rendering utilities for combinatorial structures.
 Copyright   : (c) Maciej Bendkowski, 2017-2018

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 -}
module Data.Boltzmann.System.Renderer
    ( toDotFile
    ) where

import Control.Monad

import Data.GraphViz
import Data.GraphViz.Printing
import Data.GraphViz.Attributes.Colors
import Data.GraphViz.Attributes.Complete

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
toRTree :: Structure -> IO RTree
toRTree s = do
    (t, _, _) <- toRTree' M.empty 0 s
    return t

-- | Node color map.
type ColorMap = Map String Color

toRTree' :: ColorMap
         -> Int -> Structure
         -> IO (RTree, Int, ColorMap)

toRTree' cm rk str =
    let s = name str in
    case s `M.lookup` cm of
        Nothing -> do
            col             <- randomColor
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

-- | Constructs a random rgb color.
randomColor :: IO Color
randomColor = do
    (r, g, b) <- liftM3 (,,) randomIO randomIO randomIO
    return RGB { red = r, green = g, blue = b }

-- | Given a tree structure, produces a suitable dotfile  representation.
toGraph :: Structure -> IO (DotGraph String)
toGraph t = do
    rT         <- toRTree t
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
toDotFile :: Structure -> IO Text
toDotFile s = do
    g <- toGraph s
    return $ renderDot (toDot g)
