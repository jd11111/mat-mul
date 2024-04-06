{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Array.Unboxed ( UArray, listArray )
import MatMul
import Data.Complex 

testa :: UArray Int Float; testa = listArray (0,7) [1,2,3,9,5,3,2,7]
testb :: UArray Int Float; testb = listArray (0,7) [5,3,2,7,1,2,3,9]

mata :: MyMatrix (Complex Float);
mata = GenCoMat{int_cdata = testa, c_rows=2, c_cols=2}

matb :: MyMatrix (Complex Float);
matb = GenCoMat{int_cdata = testb, c_rows=2, c_cols=2}

main :: IO ()
main =do{
print $ testa;
print $ testb;
print $ complexMatMul mata matb;
}
