{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Array.ST
import Data.Array.Unboxed
import qualified Control.Monad.ST
import qualified Data.STRef
import qualified Control.Monad
import GHC.Conc (killThread)

{- matProd :: UArray Int Float -> Int -> Int -> UArray Int Float -> Int -> Int -> UArray Int Float
matProd a ra ca b rb cb =
    let{(al,ah) = bounds a;
        (bl,bh) = bounds b;
        --{-# INLINE getA #-}
        getA i j    = a!(i*ca + j);
        --{-# INLINE getB #-}
        getB i j    = b!(i*cb + j);
        --{-# INLINE idx #-}
        idx i j     = i*cb + j}
        in runSTUArray $ do{
            arr <- newArray (0,ra*cb-1) 0 ::ST s (STUArray s Int Float);
            let{
            outer i j
             | ra <= i   = return arr
             | cb <= j   = outer (i+1) 0
             | otherwise = (do{x <- inner i j 0 0; writeArray arr (idx i j) x; outer i (j+1);})
             ;
            inner i j k y
             | ca <= k   = return y
             | otherwise = inner i j (k+1) (y + getA i k * getB k j)};
            outer 0 0}
-}

buildPair :: Control.Monad.ST.ST s (Float, Float)
buildPair = do{ arr <- Data.Array.ST.newArray (1,10) 37.0 :: Control.Monad.ST.ST s (Data.Array.ST.STUArray s Int Float);
               a <- Data.Array.ST.readArray arr 1;
               Data.Array.ST.writeArray arr 1 64.0;
               b <- Data.Array.ST.readArray arr 1;
               return (a,b)}

modifyAsST :: UArray Int Int -> UArray Int Int
modifyAsST arr = Control.Monad.ST.runST $ do{starr <- Data.Array.ST.thaw arr;
                            compute starr;
                            newarr <- Data.Array.ST.freeze starr;
                            return newarr}

compute :: Data.Array.ST.STUArray s Int Int -> Control.Monad.ST.ST s ()
compute arr = do Data.Array.ST.writeArray arr 1 64

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

sumST :: Num a => [a] -> a
sumST xs = Control.Monad.ST.runST $ do{
    n <- Data.STRef.newSTRef 0;            
    Control.Monad.forM_ xs $ \x -> do{Data.STRef.modifySTRef n (+x)};
    Data.STRef.readSTRef n}


x :: [Integer]
x = [1,2,3,4]
main :: IO ()
main =do{
print $ matMul test 2 2 test 2 2;
}