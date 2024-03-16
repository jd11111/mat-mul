{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Array.Unboxed ( UArray, listArray )
import MatMul

testa :: UArray Int Float; testa = listArray (0,3) [1,2,3,9]
testb :: UArray Int Float; testb = listArray (0,3) [5,3,2,7]

main :: IO ()
main =do{
print $ testa;
print $ testb;
print $ matMul testa 2 2 testb 2 2;
}