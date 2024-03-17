{-# LANGUAGE BangPatterns #-}
module MatMul where

import qualified Data.Array.ST
import Data.Array.Unboxed
import qualified Control.Monad.ST
import qualified Data.STRef
import qualified Control.Monad

--define custom matrix type: int_data are the entries of the matrix stored in row major order, rows and cols are obvious
data Matrix = GenMat{int_data :: UArray Int Float, rows :: Int, cols :: Int} deriving Show 

matMul :: Matrix -> Matrix -> Matrix
matMul x y = let{
    a = int_data x;
    ra = rows x;
    ca = cols x;
    b= int_data y;
    rb = rows y;
    cb=cols y;}
    in let{
        getA :: Int -> Int -> Float;
        getA i j = a!(i*ca + j); -- access the (i,j) entry of a from the row major storage
        {-# INLINE getA #-};
        getB :: Int -> Int -> Float;
        getB i j = b!(i*cb + j); -- access the (i,j) entry of b from the row major storage
        {-# INLINE getB #-};
    }
    in let !z = Data.Array.ST.runSTUArray $ do{
        arr <- Data.Array.ST.newArray (0,ra*cb-1) 0.0 :: Control.Monad.ST.ST s (Data.Array.ST.STUArray s Int Float);--zero initialised output array
        acc <- Data.STRef.newSTRef 0.0; --zero initialised accumulator variable
        Control.Monad.forM_ [0..(ra-1)] $ \i -> do{ --loop over all rows of a
            Control.Monad.forM_ [0..(cb-1)] $ \j -> do{ --loop over all columns of b
                Control.Monad.forM_ [0..(ca-1)] $ \k -> do{
                    Data.STRef.modifySTRef' acc (+ (getA i k * getB k j)); --using strict modifySTRef' (important) to accumulate (a*b)(i,j) in acc
                    };
                var <- Data.STRef.readSTRef acc;
                Data.Array.ST.writeArray arr (i*cb+j) var; --write the value in the accumulator to the appropriate position
                Data.STRef.writeSTRef acc 0.0; --set accumulator to zero for next loop
            };
        };
        return arr;}
    in GenMat{int_data = z, rows = ra, cols=cb}