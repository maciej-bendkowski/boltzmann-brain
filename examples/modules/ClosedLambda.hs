-- | Compiler: boltzmann-brain ALPHA (2016-11-27 12:17:08.507548 CET)
-- | Singularity: 1.82744026184082031250e-1
module Sampler
       (L0(..), genRandomL0, sampleL0, L1(..), genRandomL1, sampleL1,
        L2(..), genRandomL2, sampleL2, L3(..), genRandomL3, sampleL3,
        L4(..), genRandomL4, sampleL4, L5(..), genRandomL5, sampleL5)
       where
import Control.Monad (guard)
import Control.Monad.Random (RandomGen(..), Rand, getRandomR)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

data L0 = App0 L0 L0
        | Abs0 L1
        deriving Show

data L1 = App1 L1 L1
        | Abs1 L2
        | L1_0
        deriving Show

data L2 = App2 L2 L2
        | Abs2 L3
        | L2_1
        | L2_0
        deriving Show

data L3 = App3 L3 L3
        | Abs3 L4
        | L3_2
        | L3_1
        | L3_0
        deriving Show

data L4 = App4 L4 L4
        | Abs4 L5
        | L4_3
        | L4_2
        | L4_1
        | L4_0
        deriving Show

data L5 = App5 L5 L5
        | Abs5 L5
        | L5_4
        | L5_3
        | L5_2
        | L5_1
        | L5_0
        deriving Show

randomP :: RandomGen g => MaybeT (Rand g) Double
randomP = lift (getRandomR (0, 1))

genRandomL0 :: RandomGen g => Int -> MaybeT (Rand g) (L0, Int)
genRandomL0 ub
  = do guard (ub > 0)
       p <- randomP
       if p < 1.0657573463690107e-2 then
         do (x0, w0) <- genRandomL0 (ub - 1)
            (x1, w1) <- genRandomL0 (ub - 1 - w0)
            return (App0 x0 x1, 1 + w0 + w1)
         else
         do (x0, w0) <- genRandomL1 (ub - 1)
            return (Abs0 x0, 1 + w0)

genRandomL1 :: RandomGen g => Int -> MaybeT (Rand g) (L1, Int)
genRandomL1 ub
  = do guard (ub > 0)
       p <- randomP
       if p < 5.7698136749831146e-2 then
         do (x0, w0) <- genRandomL1 (ub - 1)
            (x1, w1) <- genRandomL1 (ub - 1 - w0)
            return (App1 x0 x1, 1 + w0 + w1)
         else
         if p < 0.42120526238573525 then
           do (x0, w0) <- genRandomL2 (ub - 1)
              return (Abs1 x0, 1 + w0)
           else return (L1_0, 1)

genRandomL2 :: RandomGen g => Int -> MaybeT (Rand g) (L2, Int)
genRandomL2 ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.11477083154199086 then
         do (x0, w0) <- genRandomL2 (ub - 1)
            (x1, w1) <- genRandomL2 (ub - 1 - w0)
            return (App2 x0 x1, 1 + w0 + w1)
         else
         if p < 0.41805115827102185 then
           do (x0, w0) <- genRandomL3 (ub - 1)
              return (Abs2 x0, 1 + w0)
           else
           if p < 0.7090256041061571 then return (L2_1, 1) else
             return (L2_0, 1)

genRandomL3 :: RandomGen g => Int -> MaybeT (Rand g) (L3, Int)
genRandomL3 ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.19047263002706843 then
         do (x0, w0) <- genRandomL3 (ub - 1)
            (x1, w1) <- genRandomL3 (ub - 1 - w0)
            return (App3 x0 x1, 1 + w0 + w1)
         else
         if p < 0.4740130739007773 then
           do (x0, w0) <- genRandomL4 (ub - 1)
              return (Abs3 x0, 1 + w0)
           else
           if p < 0.6493420915198577 then return (L3_2, 1) else
             if p < 0.824671109138938 then return (L3_1, 1) else
               return (L3_0, 1)

genRandomL4 :: RandomGen g => Int -> MaybeT (Rand g) (L4, Int)
genRandomL4 ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.29553192622157504 then
         do (x0, w0) <- genRandomL4 (ub - 1)
            (x1, w1) <- genRandomL4 (ub - 1 - w0)
            return (App4 x0 x1, 1 + w0 + w1)
         else
         if p < 0.5479966024853484 then
           do (x0, w0) <- genRandomL5 (ub - 1)
              return (Abs4 x0, 1 + w0)
           else
           if p < 0.6609975209747108 then return (L4_3, 1) else
             if p < 0.7739984394640731 then return (L4_2, 1) else
               if p < 0.8869993579534355 then return (L4_1, 1) else
                 return (L4_0, 1)

genRandomL5 :: RandomGen g => Int -> MaybeT (Rand g) (L5, Int)
genRandomL5 ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.40828350801454727 then
         do (x0, w0) <- genRandomL5 (ub - 1)
            (x1, w1) <- genRandomL5 (ub - 1 - w0)
            return (App5 x0 x1, 1 + w0 + w1)
         else
         if p < 0.5910275341986293 then
           do (x0, w0) <- genRandomL5 (ub - 1)
              return (Abs5 x0, 1 + w0)
           else
           if p < 0.6728221167630041 then return (L5_4, 1) else
             if p < 0.7546166993273788 then return (L5_3, 1) else
               if p < 0.8364112818917536 then return (L5_2, 1) else
                 if p < 0.9182058644561284 then return (L5_1, 1) else
                   return (L5_0, 1)

sampleL0 :: RandomGen g => Int -> Int -> Rand g L0
sampleL0 lb ub
  = do let sampler = runMaybeT (genRandomL0 ub)
       x <- sampler
       case x of
           Nothing -> sampleL0 lb ub
           Just (t', s) -> if lb <= s then return t' else sampleL0 lb ub

sampleL1 :: RandomGen g => Int -> Int -> Rand g L1
sampleL1 lb ub
  = do let sampler = runMaybeT (genRandomL1 ub)
       x <- sampler
       case x of
           Nothing -> sampleL1 lb ub
           Just (t', s) -> if lb <= s then return t' else sampleL1 lb ub

sampleL2 :: RandomGen g => Int -> Int -> Rand g L2
sampleL2 lb ub
  = do let sampler = runMaybeT (genRandomL2 ub)
       x <- sampler
       case x of
           Nothing -> sampleL2 lb ub
           Just (t', s) -> if lb <= s then return t' else sampleL2 lb ub

sampleL3 :: RandomGen g => Int -> Int -> Rand g L3
sampleL3 lb ub
  = do let sampler = runMaybeT (genRandomL3 ub)
       x <- sampler
       case x of
           Nothing -> sampleL3 lb ub
           Just (t', s) -> if lb <= s then return t' else sampleL3 lb ub

sampleL4 :: RandomGen g => Int -> Int -> Rand g L4
sampleL4 lb ub
  = do let sampler = runMaybeT (genRandomL4 ub)
       x <- sampler
       case x of
           Nothing -> sampleL4 lb ub
           Just (t', s) -> if lb <= s then return t' else sampleL4 lb ub

sampleL5 :: RandomGen g => Int -> Int -> Rand g L5
sampleL5 lb ub
  = do let sampler = runMaybeT (genRandomL5 ub)
       x <- sampler
       case x of
           Nothing -> sampleL5 lb ub
           Just (t', s) -> if lb <= s then return t' else sampleL5 lb ub
