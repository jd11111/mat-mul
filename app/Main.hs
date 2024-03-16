{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Array.Unboxed ( UArray, listArray )
import MatMul

test :: UArray Int Float; test = listArray (0,3) (repeat 2.0) 

main :: IO ()
main =do{
print $ test;
print $ matMul test 2 2 test 2 2;
}