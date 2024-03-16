{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Time.Clock
import Data.Array.Unboxed
import MatMul
import System.Random

main = do
    gen <- newStdGen
    let ns = randoms gen :: [Float]
    let !testa = listArray (0,250000-1) (take 250000 ns):: UArray Int Float
    let !testb = listArray (0,250000-1) (take 250000 ns):: UArray Int Float
    start <- getCurrentTime
    let !testc = matMul testa 500 500 testb 500 500
    end <- getCurrentTime
    putStrLn $ "matmul " ++ show (diffUTCTime end start)