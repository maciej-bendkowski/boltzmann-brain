module Lambda
       (DeBruijn(..), genRandomDeBruijn, sampleDeBruijn, Lambda(..),
        genRandomLambda, sampleLambda)
       where
import Control.Monad.Random

class Combinatorial a where
        size :: a -> Int

data DeBruijn = S DeBruijn
              | Z
              deriving Show

data Lambda = App Lambda Lambda
            | Abs Lambda
            | Index DeBruijn
            deriving Show

instance Combinatorial DeBruijn where
        size (S x0) = 1 + size x0
        size Z = 1

instance Combinatorial Lambda where
        size (App x0 x1) = 1 + size x0 + size x1
        size (Abs x0) = 1 + size x0
        size (Index x0) = 0 + size x0

randomP :: RandomGen g => Rand g Double
randomP = getRandomR (0, 1)

genRandomDeBruijn :: RandomGen g => Rand g DeBruijn
genRandomDeBruijn
  = do p <- randomP
       if p < 0.2955970764160156 then
         do x0 <- genRandomDeBruijn
            return (S x0)
         else do return Z

genRandomLambda :: RandomGen g => Rand g Lambda
genRandomLambda
  = do p <- randomP
       if p < 0.35110353598417565 then
         do x0 <- genRandomLambda
            x1 <- genRandomLambda
            return (App x0 x1)
         else
         if p < 0.6467006124001913 then
           do x0 <- genRandomLambda
              return (Abs x0)
           else
           do x0 <- genRandomDeBruijn
              return (Index x0)

sampleDeBruijn :: RandomGen g => Int -> Int -> Rand g DeBruijn
sampleDeBruijn
  = \ lb ub ->
      do x <- genRandomDeBruijn
         let s = size x
         if s < lb || ub < s then sampleDeBruijn lb ub else return x

sampleLambda :: RandomGen g => Int -> Int -> Rand g Lambda
sampleLambda
  = \ lb ub ->
      do x <- genRandomLambda
         let s = size x
         if s < lb || ub < s then sampleLambda lb ub else return x
