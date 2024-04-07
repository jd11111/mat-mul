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
import Data.Array.Unboxed
import Data.Complex

class ( Show a, RealFloat a) => MyNums a where
  myUnsafeAt ::Data.Array.Unboxed.UArray Int a -> Int -> a
  myNewArray :: (Int, Int) -> a -> Control.Monad.ST.ST s (Data.Array.ST.STUArray s Int a);
  myUnsafeWrite :: Data.Array.ST.STUArray s Int a -> Int -> a -> Control.Monad.ST.ST s ();
  myListArray :: (Int, Int) -> [a] -> Data.Array.Unboxed.UArray Int a;

instance MyNums Float where
  {-# INLINE myUnsafeAt #-}
  myUnsafeAt =Data.Array.Base.unsafeAt :: UArray Int Float -> Int -> Float;
  myNewArray = Data.Array.ST.newArray :: (Int,Int) -> Float -> Control.Monad.ST.ST s (Data.Array.ST.STUArray s Int Float);
  {-# INLINE myUnsafeWrite #-}
  myUnsafeWrite = Data.Array.Base.unsafeWrite :: Data.Array.ST.STUArray s Int Float -> Int -> Float -> Control.Monad.ST.ST s ();
  myListArray = listArray;

instance MyNums Double where
  {-# INLINE myUnsafeAt #-}
  myUnsafeAt =Data.Array.Base.unsafeAt :: UArray Int Double -> Int -> Double;
  myNewArray = Data.Array.ST.newArray :: (Int,Int) -> Double -> Control.Monad.ST.ST s (Data.Array.ST.STUArray s Int Double);
  {-# INLINE myUnsafeWrite #-}
  myUnsafeWrite = Data.Array.Base.unsafeWrite :: Data.Array.ST.STUArray s Int Double -> Int -> Double -> Control.Monad.ST.ST s ();
  myListArray = listArray;

class (ProtoMat m a, MyNums a) => RealMat m a where
  int_data :: m a -> Data.Array.Unboxed.UArray Int a
  mkRealMat :: Data.Array.Unboxed.UArray Int a -> Int -> Int -> m a

data family MyMatrix a

class ProtoMat m a where
  cols :: m a -> Int
  rows :: m a -> Int

data instance MyMatrix Float = GenFMat {intf_data :: Data.Array.Unboxed.UArray Int Float, f_rows :: Int, f_cols :: Int}

instance ProtoMat MyMatrix Float where
  cols = f_cols
  rows = f_rows

instance RealMat MyMatrix Float where
  int_data  = intf_data
  mkRealMat a r c | r*c -1 == snd (Data.Array.Base.bounds a) = GenFMat{intf_data = a, f_rows = r, f_cols = c}| otherwise = undefined

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

instance ProtoMat MyMatrix Double where
  cols = d_cols
  rows = d_rows

instance RealMat MyMatrix Double where
  int_data = intd_data
  mkRealMat a r c | r*c -1 == snd (Data.Array.Base.bounds a) && fst (Data.Array.Base.bounds a) ==0= GenDMat{intd_data = a, d_rows = r, d_cols = c}| otherwise = undefined

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
      getA i j = myUnsafeAt a (i * ca + j); -- access the (i,j) entry of a from the row major storage
      {-# INLINE getB #-};
      getB i j = myUnsafeAt b (i * cb + j); -- access the (i,j) entry of b from the row major storage
      !z = Data.Array.ST.runSTUArray $ do{
                arr <- myNewArray (0, ra * cb - 1) 0.0;
                Control.Monad.forM_ [0 .. (ra - 1)] $ \i -> do{
                  Control.Monad.forM_ [0 .. (cb - 1)] $ \j -> do{
                    let inner k !var | ca <= k = var | otherwise = inner (k + 1) (var + getA i k * getB k j)
                    in myUnsafeWrite arr (i * cb + j) (inner 0 0.0);
                  };
                };
              return arr};
      } in if ca == rb then mkRealMat z ra cb else undefined

realTrace :: RealMat m a => m a -> a
realTrace x =
  let {a = int_data x;
      ca = cols x;
      ra = rows x;}
   in if ca == ra then sum [ myUnsafeAt a (i * ca + i) | i <- range (0, ca - 1)] else undefined

realScaMul :: RealMat m a => a -> m a -> m a
realScaMul alpha x = let a = int_data x; ra = rows x; ca = cols x; z = myListArray (0, ra*ca-1) [ alpha * myUnsafeAt a (i*ca +j) | i <- range (0, ra-1), j <- range(0,ca-1)] in mkRealMat z ra ca

data instance (MyNums a) => MyMatrix (Complex a)  = GenCoMat {int_cdata :: Data.Array.Unboxed.UArray Int a, c_rows :: Int, c_cols :: Int}

mkComplexMat:: (IArray UArray a, MyNums a) => UArray Int a -> Int -> Int -> MyMatrix (Complex a)
mkComplexMat a r c | 2*r*c -1 == snd (Data.Array.Base.bounds a) && fst (Data.Array.Base.bounds a) ==0= GenCoMat{int_cdata = a, c_rows = r, c_cols = c}| otherwise = undefined

instance (MyNums a) => ProtoMat MyMatrix (Complex a) where
  cols = c_cols
  rows = c_rows

{-# INLINE complexMatMul #-}
complexMatMul :: (MyNums a) => MyMatrix (Complex a) -> MyMatrix (Complex a) -> MyMatrix (Complex a)
complexMatMul x y =
  let{a = int_cdata x;
      ra = rows x;
      ca = cols x;
      b = int_cdata y;
      rb = rows y;
      cb = cols y;
      {-# INLINE getReA #-};
      getReA i j = myUnsafeAt a (2*(i * ca + j));
      {-# INLINE getImA #-};
      getImA i j = myUnsafeAt a (2*(i * ca + j)+1);
      {-# INLINE getReB #-};
      getReB i j = myUnsafeAt b (2*(i * ca + j));
      {-# INLINE getImB #-};
      getImB i j = myUnsafeAt b (2*(i * ca + j)+1);
      !z = Data.Array.ST.runSTUArray $ do{
                arr <- myNewArray (0, 2*ra * cb - 1) 0.0;
                Control.Monad.forM_ [0 .. (ra - 1)] $ \i -> do{
                  Control.Monad.forM_ [0 .. (cb - 1)] $ \j -> do{
                    let {inner k !var | ca <= k = var | otherwise = inner (k + 1) (var + (getReA i k :+ getImA i k) * (getReB k j :+ getImB k j)); res = inner 0 0.0;}
                    in do{ myUnsafeWrite arr (2*(i * cb + j)) (realPart res); myUnsafeWrite arr (2*(i * cb + j)+1) (imagPart res);}
                  };
                };
              return arr};
      } in if ca == rb then GenCoMat{int_cdata = z, c_rows = ra, c_cols = cb} else undefined


complexTrace :: (MyNums a) => MyMatrix (Complex a) -> Complex a
complexTrace x =
  let {a = int_cdata x;
      ca = cols x;
      ra = rows x;}
   in if ca == ra then sum [ myUnsafeAt a (2*(i * ca + i)) | i <- range (0, ca - 1)] :+ sum [ myUnsafeAt a (2*(i * ca + i)+1) | i <- range (0, ca - 1)] else undefined

{-# INLINE complexScaMul #-}
complexScaMul :: (MyNums a) => Complex a -> MyMatrix (Complex a) -> MyMatrix (Complex a)
complexScaMul alpha x =
  let{a = int_cdata x;
      ra = rows x;
      ca = cols x;
      {-# INLINE getReA #-};
      getReA i j = myUnsafeAt a (2*(i * ca + j));
      {-# INLINE getImA #-};
      getImA i j = myUnsafeAt a (2*(i * ca + j)+1);
      !z = Data.Array.ST.runSTUArray $ do{
                arr <- myNewArray (0, 2*ra * ca - 1) 0.0;
                Control.Monad.forM_ [0 .. (ra - 1)] $ \i -> do{
                  Control.Monad.forM_ [0 .. (ca - 1)] $ \j -> do{
                    let {var = alpha * (getReA i j :+ getImA i j)}
                    in do{ myUnsafeWrite arr (2*(i * ca + j)) (realPart var); myUnsafeWrite arr (2*(i * ca + j)+1) (imagPart var);}
                  };
                };
              return arr};
      } in GenCoMat{int_cdata = z, c_rows = ra, c_cols = ca}

instance (MyNums a) => Matrix MyMatrix (Complex a) where 
  matMul = complexMatMul
  scaMul = complexScaMul
  matTrace = complexTrace
