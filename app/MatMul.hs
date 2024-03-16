{-# LANGUAGE BangPatterns #-}
module MatMul where

import qualified Data.Array.ST
import Data.Array.Unboxed
import qualified Control.Monad.ST
import qualified Data.STRef
import qualified Control.Monad
import qualified Data.Array

matMul :: UArray Int Float -> Int -> Int -> UArray Int Float -> Int -> Int -> UArray Int Float
matMul a ra ca b rb cb =let{
    getA :: Int -> Int -> Float;
    getA i j = a!(i*ca + j); -- access the (i,j) entry of a from the row major storage
    {-# INLINE getA #-};
    getB :: Int -> Int -> Float;
    getB i j = b!(i + j*rb); -- access the (i,j) entry of b from the column major storage
    {-# INLINE getB #-};
    }
    in Data.Array.ST.runSTUArray $ do{
        arr <- Data.Array.ST.newArray (0,ra*cb-1) 0.0 :: Control.Monad.ST.ST s (Data.Array.ST.STUArray s Int Float);--zero initialised output array
        Control.Monad.forM_ [0..(ra-1)] $ \i -> do{ --loop over all rows of a
            Control.Monad.forM_ [0..(cb-1)] $ \j -> do{ --loop over all columns of b
                acc <- Data.STRef.newSTRef 0.0; --zero initialised accumulator variable
                Control.Monad.forM_ [0..(ca-1)] $ \k -> do{
                    let{var = (getA i k * getB k j)}
                    in Data.STRef.modifySTRef' acc (+ var); --using strict modifySTRef' (important) to accumulate (a*b)(i,j) in acc
                    };
                z <- Data.STRef.readSTRef acc;
                Data.Array.ST.writeArray arr (i+ra*j) z; --write the value in the accumulator to the appropriate position
            };
        };
        return arr;
    }