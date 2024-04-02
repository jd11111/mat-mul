{-# LANGUAGE BangPatterns #-}

module MatMul where

import qualified Control.Monad
import qualified Control.Monad.ST
import qualified Data.Array.Base
import qualified Data.Array.ST
import qualified Data.Array.Unboxed

-- import qualified Data.STRef

-- define custom matrix type: int_data are the entries of the matrix stored in row major order, rows and cols are obvious
data Matrix = GenMat {int_data :: Data.Array.Unboxed.UArray Int Float, rows :: Int, cols :: Int} deriving (Show)

matMul :: Matrix -> Matrix -> Matrix
matMul x y =
  let a = int_data x
      ra = rows x
      ca = cols x
      b = int_data y
      rb = rows y
      cb = cols y
   in let {-# INLINE getA #-}
          getA :: Int -> Int -> Float
          getA i j = Data.Array.Base.unsafeAt a (i * ca + j) -- access the (i,j) entry of a from the row major storage
          {-# INLINE getB #-}
          getB :: Int -> Int -> Float
          getB i j = Data.Array.Base.unsafeAt b (i * cb + j) -- access the (i,j) entry of b from the row major storage
       in let !z = Data.Array.ST.runSTUArray $ do
                arr <- Data.Array.ST.newArray (0, ra * cb - 1) 0.0 :: Control.Monad.ST.ST s (Data.Array.ST.STUArray s Int Float) -- zero initialised output array
                Control.Monad.forM_ [0 .. (ra - 1)] $ \i -> do
                  -- loop over all rows of a
                  Control.Monad.forM_ [0 .. (cb - 1)] $ \j -> do
                    -- loop over all columns of b
                    Control.Monad.forM_ [0 .. (ca - 1)] $ \k -> do
                      var <- Data.Array.Base.unsafeRead arr (i * cb + j)
                      Data.Array.Base.unsafeWrite arr (i * cb + j) (var + (getA i k * getB k j))
                return arr
           in GenMat {int_data = z, rows = ra, cols = cb}
