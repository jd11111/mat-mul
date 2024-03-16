{-# LANGUAGE BangPatterns #-}
module Main where
import Data.Time.Clock
import Data.Array.Unboxed
import MatMul
import System.Random
main = do
    gen <- newStdGen
    let ns = randoms gen :: [Float]
    let !testa = listArray (0,1000000-1) (take 1000000 ns):: UArray Int Float
    let !testb = listArray (0,1000000-1) (take 1000000 ns):: UArray Int Float
    start <- getCurrentTime
    let !testc = matMul testa 1000 1000 testb 1000 1000
    end <- getCurrentTime
    putStrLn $ "matmul " ++ show (diffUTCTime end start)