module Binary (B(..), genRandomB, sampleB) where
import Control.Monad.Random

class Combinatorial a where
        size :: a -> Int

data B = Leaf
       | Node B B
       deriving Show

instance Combinatorial B where
        size Leaf = 1
        size (Node x0 x1) = 1 + size x0 + size x1

randomP :: RandomGen g => Rand g Double
randomP = getRandomR (0, 1)

genRandomB :: RandomGen g => Rand g B
genRandomB
  = do p <- randomP
       if p < 0.5007072019510743 then do return Leaf else
         do x0 <- genRandomB
            x1 <- genRandomB
            return (Node x0 x1)

sampleB :: RandomGen g => Int -> Int -> Rand g B
sampleB
  = \ lb ub ->
      do x <- genRandomB
         let s = size x
         if s < lb || ub < s then sampleB lb ub else return x
