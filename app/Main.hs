{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Array.ST
import Data.Array.Unboxed
import qualified Control.Monad.ST
import qualified Data.STRef
import qualified Control.Monad


test :: UArray Int Float; test = listArray (0,4) (repeat 2.0) 


matMul :: UArray Int Float -> Int -> Int -> UArray Int Float -> Int -> Int -> UArray Int Float
matMul a ra ca b rb cb =let{
    getA :: Int -> Int -> Float;
    getA i j = a!(i*ca + j);
    getB :: Int -> Int -> Float;
    getB i j = b!(i*cb + j);
    }
    in Control.Monad.ST.runST $ do{
    arr <- Data.Array.ST.newArray (0,ra*cb-1) 0.0 :: Control.Monad.ST.ST s (Data.Array.ST.STUArray s Int Float);           
    
    let z = Control.Monad.ST.runST $ do{
        acc <- Data.STRef.newSTRef 0.0;
        Control.Monad.forM_ [0..(ca-1)] $ \k -> do{Data.STRef.modifySTRef acc (+ getA 0 k)};
        Data.STRef.readSTRef acc
    } in
    Data.Array.ST.writeArray arr 1 z;
    Data.Array.ST.freeze arr;
    }

main :: IO ()
main =do{
print $ matMul test 2 2 test 2 2;
}