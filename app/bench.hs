{-# LANGUAGE BangPatterns #-}
module Main where
import Data.Time.Clock
import Data.Array.Unboxed
import MatMul
import System.Random

n::Int
n=500

m = n^2
main = do
    gen <- newStdGen
    print m;
    let ns = randoms gen :: [Float]
    let !testa = listArray (0,m-1) (take m ns):: UArray Int Float
    let !testb = listArray (0,m-1) (take m ns):: UArray Int Float
    let !mata = GenMat{int_data = testa, rows=n, cols=n};
    let !matb = GenMat{int_data = testb, rows=n, cols=n};
    start <- getCurrentTime
    let !testc = matMul mata matb;
    end <- getCurrentTime
    putStrLn $ "matmul " ++ show (diffUTCTime end start)