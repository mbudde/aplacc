{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Interpreter

import Tail.Primitives


test1 :: Acc (Scalar Double)
test1 = let v0 = iota 3 in
        i2d (Acc.fold (+) 0 v0)

test2 :: Acc (Scalar Double)
test2 = let v0 = iota 30 Acc.++ iota 22 in
        let v2 = Acc.use (Acc.fromList (Z :. 11) [1,2,23,3,4,5,344,6,7,7,5])
                 Acc.++ Acc.map (\v3 -> 5 + v3) v0 in
        i2d (Acc.fold (\v4 -> \v5 -> max v5 v4) 0
             (Acc.drop 3 $ Acc.map (\v3 -> 5+v3) v2))

test3 :: Acc (Scalar Double)
test3 = i2d
      $ Acc.fold (+) 0
      $ Acc.map (\v0 -> v0 + 1000)
      $ Acc.use $ Acc.fromList (Z :. 2) [300, 20] :: Vector Int


main = do putStrLn $ show $ run test1
          putStrLn $ show $ run test2
          putStrLn $ show $ run test3
