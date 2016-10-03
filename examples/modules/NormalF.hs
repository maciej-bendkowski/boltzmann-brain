module NormalF
       (D(..), genRandomD, sampleD, M(..), genRandomM, sampleM, N(..),
        genRandomN, sampleN)
       where
import Control.Monad.Random

class Combinatorial a where
        size :: a -> Int

data D = S D
       | Z
       deriving Show

data M = App M N
       | Index D
       deriving Show

data N = Neutral M
       | Abs N
       deriving Show

instance Combinatorial D where
        size (S x0) = 1 + size x0
        size Z = 1

instance Combinatorial M where
        size (App x0 x1) = 1 + size x0 + size x1
        size (Index x0) = 0 + size x0

instance Combinatorial N where
        size (Neutral x0) = 0 + size x0
        size (Abs x0) = 1 + size x0

randomP :: RandomGen g => Rand g Double
randomP = getRandomR (0, 1)

genRandomD :: RandomGen g => Rand g D
genRandomD
  = do p <- randomP
       if p < 0.33333301544189453 then
         do x0 <- genRandomD
            return (S x0)
         else do return Z

genRandomM :: RandomGen g => Rand g M
genRandomM
  = do p <- randomP
       if p < 0.4988611376022971 then
         do x0 <- genRandomM
            x1 <- genRandomN
            return (App x0 x1)
         else
         do x0 <- genRandomD
            return (Index x0)

genRandomN :: RandomGen g => Rand g N
genRandomN
  = do p <- randomP
       if p < 0.6666676510292284 then
         do x0 <- genRandomM
            return (Neutral x0)
         else
         do x0 <- genRandomN
            return (Abs x0)

sampleD :: RandomGen g => Int -> Int -> Rand g D
sampleD
  = \ lb ub ->
      do x <- genRandomD
         let s = size x
         if s < lb || ub < s then sampleD lb ub else return x

sampleM :: RandomGen g => Int -> Int -> Rand g M
sampleM
  = \ lb ub ->
      do x <- genRandomM
         let s = size x
         if s < lb || ub < s then sampleM lb ub else return x

sampleN :: RandomGen g => Int -> Int -> Rand g N
sampleN
  = \ lb ub ->
      do x <- genRandomN
         let s = size x
         if s < lb || ub < s then sampleN lb ub else return x
