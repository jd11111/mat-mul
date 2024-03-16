{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Array.ST
import Data.Array.Unboxed
import qualified Control.Monad.ST
import qualified Data.STRef
import qualified Control.Monad


test :: UArray Int Float; test = listArray (0,3) (repeat 2.0) 

matMul :: UArray Int Float -> Int -> Int -> UArray Int Float -> Int -> Int -> UArray Int Float
matMul a ra ca b rb cb =let{
    getA :: Int -> Int -> Float;
    getA i j = a!(i*ca + j);
    getB :: Int -> Int -> Float;
    getB i j = b!(i*cb + j);
    }
    in Control.Monad.ST.runST $ do{
    arr <- Data.Array.ST.newArray (0,ra*cb-1) 0.0 :: Control.Monad.ST.ST s (Data.Array.ST.STUArray s Int Float);
    Control.Monad.forM_ [0..(ra-1)] $ \i -> do{
        Control.Monad.forM_ [0..(cb-1)] $ \j -> do{
            let z = Control.Monad.ST.runST $ do{
            acc <- Data.STRef.newSTRef 0.0;
            Control.Monad.forM_ [0..(ca-1)] $ \k -> do{Data.STRef.modifySTRef acc (+ (getA i k + getB k j))};
            Data.STRef.readSTRef acc
            } in
            Data.Array.ST.writeArray arr (i+ra*j) z;
        };
    };
    Data.Array.ST.freeze arr;
    }

main :: IO ()
main =do{
print $ test;
print $ matMul test 2 2 test 2 2;
}