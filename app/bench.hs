{-# LANGUAGE BangPatterns #-}

import Data.Array.Unboxed
import Data.Time.Clock
import MatMul
import System.Random
import Data.Complex

n :: Int
n = 500

m = n ^ 2 ::Int

main :: IO()
main = do
  gen <- newStdGen
  print "matrix size (square):"
  print n
  let ns = randoms gen :: [Float]
  let !testa = listArray (0, m - 1) (take m ns) :: UArray Int Float
  let !testb = listArray (0, m - 1) (take m ns) :: UArray Int Float
  let !mata = mkRealMat testa n n :: MyMatrix Float;
  let !matb = mkRealMat testb n n:: MyMatrix Float;
  start <- getCurrentTime
  let !testc = matMul mata matb;
  end <- getCurrentTime
  putStrLn $ "matmul floats:" ++ show (diffUTCTime end start)
  print $ realTrace (scaMul 5.0 mata);
  let ns = randoms gen :: [Double]
  let !testad = listArray (0, m - 1) (take m ns) :: UArray Int Double
  let !testbd = listArray (0, m - 1) (take m ns) :: UArray Int Double
  let !matad = mkRealMat testad n n:: MyMatrix Double;
  let !matbd = mkRealMat testbd n n:: MyMatrix Double;
  start <- getCurrentTime
  let !testc = matMul matad matbd;
  end <- getCurrentTime
  putStrLn $ "matmul doubles:" ++ show (diffUTCTime end start)
  print $ realTrace (scaMul 5.0 matad);
  let ns = randoms gen :: [Float]
  let !testca = listArray (0, 2*m - 1) (take (2*m) ns) :: UArray Int Float
  let !testcb = listArray (0, 2*m - 1) (take (2*m) ns) :: UArray Int Float
  let !matca = mkComplexMat testca n n;
  let !matcb = mkComplexMat testcb n n;
  start <- getCurrentTime
  let !testc = matMul matca matcb;
  end <- getCurrentTime;
  putStrLn $ "matmul complex: " ++ show (diffUTCTime end start);
  print $ matTrace (scaMul 5.0 testc);
  print $ matTrace (scaMul (5.0 :+ 2.32) testc);


