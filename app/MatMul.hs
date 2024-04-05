{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module MatMul where

import qualified Control.Monad
import qualified Control.Monad.ST
import qualified Data.Array.Base
import qualified Data.Array.ST
import qualified Data.Array.Unboxed
import Data.Array.Unboxed
import Data.Complex
import qualified Data.Array.Base as Data.Array.Unboxed

class (Num a, Show a, Fractional a) => MyNums a where
  myUnsafeAt ::Data.Array.Unboxed.UArray Int a -> Int -> a
  myNewArray :: (Int, Int) -> a -> Control.Monad.ST.ST s (Data.Array.ST.STUArray s Int a);
  myUnsafeWrite :: Data.Array.ST.STUArray s Int a -> Int -> a -> Control.Monad.ST.ST s ();
  myListArray :: (Int, Int) -> [a] -> Data.Array.Unboxed.UArray Int a;

instance MyNums Float where
  myUnsafeAt =Data.Array.Base.unsafeAt :: UArray Int Float -> Int -> Float;
  myNewArray = Data.Array.ST.newArray :: (Int,Int) -> Float -> Control.Monad.ST.ST s (Data.Array.ST.STUArray s Int Float);
  myUnsafeWrite = Data.Array.Base.unsafeWrite :: Data.Array.ST.STUArray s Int Float -> Int -> Float -> Control.Monad.ST.ST s ();
  myListArray = listArray;

instance MyNums Double where
  myUnsafeAt =Data.Array.Base.unsafeAt :: UArray Int Double -> Int -> Double;
  myNewArray = Data.Array.ST.newArray :: (Int,Int) -> Double -> Control.Monad.ST.ST s (Data.Array.ST.STUArray s Int Double);
  myUnsafeWrite = Data.Array.Base.unsafeWrite :: Data.Array.ST.STUArray s Int Double -> Int -> Double -> Control.Monad.ST.ST s ();
  myListArray = listArray;

class (MyNums a) => RealMat m a where
  cols :: m a -> Int
  rows :: m a -> Int 
  int_data :: m a -> Data.Array.Unboxed.UArray Int a
  mkRealMat :: Data.Array.Unboxed.UArray Int a -> Int -> Int -> m a

data family MyMatrix a

data instance MyMatrix Float = GenFMat {intf_data :: Data.Array.Unboxed.UArray Int Float, f_rows :: Int, f_cols :: Int}

instance RealMat MyMatrix Float where
  cols = f_cols
  rows = f_rows
  int_data  = intf_data
  mkRealMat a r c = GenFMat{intf_data = a, f_rows = r, f_cols = c}

class Matrix m a where
  matMul :: m a -> m a -> m a
  scaMul :: a -> m a -> m a 
  matTrace :: m a -> a 

instance Matrix MyMatrix Float where
  matMul = realMatMul
  scaMul = realScaMul
  matTrace = realTrace

instance Matrix MyMatrix Double where
  matMul = realMatMul
  scaMul = realScaMul
  matTrace = realTrace

data instance MyMatrix Double = GenDMat {intd_data :: Data.Array.Unboxed.UArray Int Double, d_rows :: Int, d_cols :: Int}

instance RealMat MyMatrix Double where
  cols = d_cols
  rows = d_rows
  int_data = intd_data
  mkRealMat a r c = GenDMat{intd_data = a, d_rows = r, d_cols = c}

data instance MyMatrix (Complex a)  = GenCoMat {intc_data :: (Data.Array.Unboxed.UArray Int a,Data.Array.Unboxed.UArray Int a), c_rows :: Int, c_cols :: Int}


-- define custom matrix type: int_data are the entries of the matrix stored in row major order, rows and cols are obvious
--data (MyNums a) => RealMatrix a = GenMat {int_data :: Data.Array.Unboxed.UArray Int a, rows :: Int, cols :: Int}
--data ComplexMatrix = GenCompMat {re_data :: Data.Array.Unboxed.UArray Int Float, im_data :: Data.Array.Unboxed.UArray Int Float, cm_rows :: Int, cm_cols :: Int} deriving (Show)

{-# INLINE realMatMul #-}
realMatMul :: RealMat m a => m a -> m a -> m a
realMatMul x y =
  let{a = int_data x;
      ra = rows x;
      ca = cols x;
      b = int_data y;
      rb = rows y;
      cb = cols y;
       {-# INLINE getA #-};
      --getA :: MyNums t =>  Int -> Int -> t;
      getA i j = myUnsafeAt a (i * ca + j); -- access the (i,j) entry of a from the row major storage
      {-# INLINE getB #-};
      --getB :: Int -> Int -> t;
      getB i j = myUnsafeAt b (i * cb + j); -- access the (i,j) entry of b from the row major storage
      !z = Data.Array.ST.runSTUArray $ do{
                arr <- myNewArray (0, ra * cb - 1) 0.0;-- :: Control.Monad.ST.ST s (Data.Array.ST.STUArray s Int Float); -- zero initialised output array
                Control.Monad.forM_ [0 .. (ra - 1)] $ \i -> do{
                  -- loop over all rows of a
                  Control.Monad.forM_ [0 .. (cb - 1)] $ \j -> do{
                    -- loop over all columns of b
                    let inner k !var | ca <= k = var | otherwise = inner (k + 1) (var + getA i k * getB k j)
                    in myUnsafeWrite arr (i * cb + j) (inner 0 0.0);
                  };
                };
              return arr};
      } in mkRealMat z ra cb

realTrace :: RealMat m a => m a -> a
realTrace x =
  let a = int_data x
      ca = cols x
   in sum [ myUnsafeAt a (i * ca + i) | i <- range (0, ca - 1)]

realScaMul :: RealMat m a => a -> m a -> m a
realScaMul alpha x = let a = int_data x; ra = rows x; ca = cols x; z = myListArray (0, ra*ca-1) [ alpha * myUnsafeAt a (i*ca +j) | i <- range (0, ra-1), j <- range(0,ca-1)] in mkRealMat z ra ca
