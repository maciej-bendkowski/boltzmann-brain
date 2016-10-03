module Motzkin (M(..), genRandomM, sampleM) where
import Control.Monad.Random

class Combinatorial a where
        size :: a -> Int

data M = Leaf
       | Unary M
       | Binary M M
       deriving Show

instance Combinatorial M where
        size Leaf = 1
        size (Unary x0) = 1 + size x0
        size (Binary x0 x1) = 1 + size x0 + size x1

randomP :: RandomGen g => Rand g Double
randomP = getRandomR (0, 1)

genRandomM :: RandomGen g => Rand g M
genRandomM
  = do p <- randomP
       if p < 0.3341408333975344 then do return Leaf else
         if p < 0.667473848839429 then
           do x0 <- genRandomM
              return (Unary x0)
           else
           do x0 <- genRandomM
              x1 <- genRandomM
              return (Binary x0 x1)

sampleM :: RandomGen g => Int -> Int -> Rand g M
sampleM
  = \ lb ub ->
      do x <- genRandomM
         let s = size x
         if s < lb || ub < s then sampleM lb ub else return x
