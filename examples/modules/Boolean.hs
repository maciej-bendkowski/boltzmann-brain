-- | Compiler: Boltzmann brain v1.3
-- | Singularity: 0.173015595713397
-- | System type: algebraic
-- | System size: 2
-- | Constructors: 6
module Boolean
       (Fun(..), Index(..), genRandomFun, genRandomIndex,
        genRandomFunList, genRandomIndexList, sampleFun, sampleIndex,
        sampleFunList, sampleIndexList, sampleFunIO, sampleIndexIO,
        sampleFunListIO, sampleIndexListIO)
       where
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Random
       (RandomGen(..), Rand, getRandomR, evalRandIO)

data Fun = Variable Index
         | And Fun Fun
         | Or Fun Fun
         | Neg Fun
         deriving Show

data Index = Succ Index
           | Zero
           deriving Show

randomP :: RandomGen g => MaybeT (Rand g) Double
randomP = lift (getRandomR (0, 1))

genRandomFun :: RandomGen g => Int -> MaybeT (Rand g) (Fun, Int)
genRandomFun ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.4134922021473407 then
         do (x0, w0) <- genRandomIndex ub
            return (Variable x0, w0)
         else
         if p < 0.739444541501842 then
           do (x0, w0) <- genRandomFun (ub - 1)
              (x1, w1) <- genRandomFun (ub - 1 - w0)
              return (And x0 x1, 1 + w1 + w0)
           else
           if p < 0.8269844042896664 then
             do (x0, w0) <- genRandomFun (ub - 1)
                (x1, w1) <- genRandomFun (ub - 1 - w0)
                return (Or x0 x1, 1 + w1 + w0)
             else
             do (x0, w0) <- genRandomFun (ub - 1)
                return (Neg x0, 1 + w0)

genRandomIndex ::
                 RandomGen g => Int -> MaybeT (Rand g) (Index, Int)
genRandomIndex ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.173015595713397 then
         do (x0, w0) <- genRandomIndex (ub - 1)
            return (Succ x0, 1 + w0)
         else return (Zero, 1)

genRandomFunList ::
                   RandomGen g => Int -> MaybeT (Rand g) ([Fun], Int)
genRandomFunList ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.505965155492893 then
         do (x, w) <- genRandomFun ub
            (xs, ws) <- genRandomFunList (ub - w)
            return (x : xs, w + ws)
         else return ([], 0)

genRandomIndexList ::
                     RandomGen g => Int -> MaybeT (Rand g) ([Index], Int)
genRandomIndexList ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.209212646354578 then
         do (x, w) <- genRandomIndex ub
            (xs, ws) <- genRandomIndexList (ub - w)
            return (x : xs, w + ws)
         else return ([], 0)

sampleFun :: RandomGen g => Int -> Int -> Rand g Fun
sampleFun lb ub
  = do sample <- runMaybeT (genRandomFun ub)
       case sample of
           Nothing -> sampleFun lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sampleFun lb ub

sampleIndex :: RandomGen g => Int -> Int -> Rand g Index
sampleIndex lb ub
  = do sample <- runMaybeT (genRandomIndex ub)
       case sample of
           Nothing -> sampleIndex lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sampleIndex lb ub

sampleFunIO :: Int -> Int -> IO Fun
sampleFunIO lb ub = evalRandIO (sampleFun lb ub)

sampleIndexIO :: Int -> Int -> IO Index
sampleIndexIO lb ub = evalRandIO (sampleIndex lb ub)

sampleFunListIO :: Int -> Int -> IO [Fun]
sampleFunListIO lb ub = evalRandIO (sampleFunList lb ub)

sampleIndexListIO :: Int -> Int -> IO [Index]
sampleIndexListIO lb ub = evalRandIO (sampleIndexList lb ub)