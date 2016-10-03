module ClosedLambda
       (L0(..), genRandomL0, sampleL0, L1(..), genRandomL1, sampleL1,
        L2(..), genRandomL2, sampleL2, L3(..), genRandomL3, sampleL3,
        L4(..), genRandomL4, sampleL4, L5(..), genRandomL5, sampleL5)
       where
import Control.Monad.Random

class Combinatorial a where
        size :: a -> Int

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

instance Combinatorial L0 where
        size (App0 x0 x1) = 1 + size x0 + size x1
        size (Abs0 x0) = 1 + size x0

instance Combinatorial L1 where
        size (App1 x0 x1) = 1 + size x0 + size x1
        size (Abs1 x0) = 1 + size x0
        size L1_0 = 1

instance Combinatorial L2 where
        size (App2 x0 x1) = 1 + size x0 + size x1
        size (Abs2 x0) = 1 + size x0
        size L2_1 = 1
        size L2_0 = 1

instance Combinatorial L3 where
        size (App3 x0 x1) = 1 + size x0 + size x1
        size (Abs3 x0) = 1 + size x0
        size L3_2 = 1
        size L3_1 = 1
        size L3_0 = 1

instance Combinatorial L4 where
        size (App4 x0 x1) = 1 + size x0 + size x1
        size (Abs4 x0) = 1 + size x0
        size L4_3 = 1
        size L4_2 = 1
        size L4_1 = 1
        size L4_0 = 1

instance Combinatorial L5 where
        size (App5 x0 x1) = 1 + size x0 + size x1
        size (Abs5 x0) = 1 + size x0
        size L5_4 = 1
        size L5_3 = 1
        size L5_2 = 1
        size L5_1 = 1
        size L5_0 = 1

randomP :: RandomGen g => Rand g Double
randomP = getRandomR (0, 1)

genRandomL0 :: RandomGen g => Rand g L0
genRandomL0
  = do p <- randomP
       if p < 1.0657573463690109e-2 then
         do x0 <- genRandomL0
            x1 <- genRandomL0
            return (App0 x0 x1)
         else
         do x0 <- genRandomL1
            return (Abs0 x0)

genRandomL1 :: RandomGen g => Rand g L1
genRandomL1
  = do p <- randomP
       if p < 5.769813674983114e-2 then
         do x0 <- genRandomL1
            x1 <- genRandomL1
            return (App1 x0 x1)
         else
         if p < 0.42120526238573536 then
           do x0 <- genRandomL2
              return (Abs1 x0)
           else do return L1_0

genRandomL2 :: RandomGen g => Rand g L2
genRandomL2
  = do p <- randomP
       if p < 0.11477083154199089 then
         do x0 <- genRandomL2
            x1 <- genRandomL2
            return (App2 x0 x1)
         else
         if p < 0.4180511582710219 then
           do x0 <- genRandomL3
              return (Abs2 x0)
           else
           if p < 0.7090256041061571 then do return L2_1 else do return L2_0

genRandomL3 :: RandomGen g => Rand g L3
genRandomL3
  = do p <- randomP
       if p < 0.19047263002706852 then
         do x0 <- genRandomL3
            x1 <- genRandomL3
            return (App3 x0 x1)
         else
         if p < 0.4740130739007774 then
           do x0 <- genRandomL4
              return (Abs3 x0)
           else
           if p < 0.6493420915198577 then do return L3_2 else
             if p < 0.8246711091389379 then do return L3_1 else do return L3_0

genRandomL4 :: RandomGen g => Rand g L4
genRandomL4
  = do p <- randomP
       if p < 0.29553192622157526 then
         do x0 <- genRandomL4
            x1 <- genRandomL4
            return (App4 x0 x1)
         else
         if p < 0.5479966024853486 then
           do x0 <- genRandomL5
              return (Abs4 x0)
           else
           if p < 0.660997520974711 then do return L4_3 else
             if p < 0.7739984394640733 then do return L4_2 else
               if p < 0.8869993579534355 then do return L4_1 else do return L4_0

genRandomL5 :: RandomGen g => Rand g L5
genRandomL5
  = do p <- randomP
       if p < 0.40828350801454766 then
         do x0 <- genRandomL5
            x1 <- genRandomL5
            return (App5 x0 x1)
         else
         if p < 0.5910275341986296 then
           do x0 <- genRandomL5
              return (Abs5 x0)
           else
           if p < 0.6728221167630043 then do return L5_4 else
             if p < 0.7546166993273791 then do return L5_3 else
               if p < 0.8364112818917537 then do return L5_2 else
                 if p < 0.9182058644561285 then do return L5_1 else do return L5_0

sampleL0 :: RandomGen g => Int -> Int -> Rand g L0
sampleL0
  = \ lb ub ->
      do x <- genRandomL0
         let s = size x
         if s < lb || ub < s then sampleL0 lb ub else return x

sampleL1 :: RandomGen g => Int -> Int -> Rand g L1
sampleL1
  = \ lb ub ->
      do x <- genRandomL1
         let s = size x
         if s < lb || ub < s then sampleL1 lb ub else return x

sampleL2 :: RandomGen g => Int -> Int -> Rand g L2
sampleL2
  = \ lb ub ->
      do x <- genRandomL2
         let s = size x
         if s < lb || ub < s then sampleL2 lb ub else return x

sampleL3 :: RandomGen g => Int -> Int -> Rand g L3
sampleL3
  = \ lb ub ->
      do x <- genRandomL3
         let s = size x
         if s < lb || ub < s then sampleL3 lb ub else return x

sampleL4 :: RandomGen g => Int -> Int -> Rand g L4
sampleL4
  = \ lb ub ->
      do x <- genRandomL4
         let s = size x
         if s < lb || ub < s then sampleL4 lb ub else return x

sampleL5 :: RandomGen g => Int -> Int -> Rand g L5
sampleL5
  = \ lb ub ->
      do x <- genRandomL5
         let s = size x
         if s < lb || ub < s then sampleL5 lb ub else return x
