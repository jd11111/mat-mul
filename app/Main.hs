{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Array.Unboxed ( UArray, listArray )
import MatMul

testa :: UArray Int Float; testa = listArray (0,3) [1,2,3,9]
testb :: UArray Int Float; testb = listArray (0,3) [5,3,2,7]

mata :: Matrix
mata = GenMat{int_data = testa, rows=2, cols=2}

matb :: Matrix
matb = GenMat{int_data = testb, rows=2, cols=2}

main :: IO ()
main =do{
print $ testa;
print $ testb;
print $ matMul mata matb;
}