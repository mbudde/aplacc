module Tail.Primitives where

import Data.Array.Accelerate as Acc

iota :: Int -> Acc (Vector Int)
iota n = Acc.use $ Acc.fromList (Z :. n) [1..]

i2d :: (Elt a, Elt b, IsIntegral a, IsNum b)
    => Acc (Scalar a) -> Acc (Scalar b)
i2d = unit . Acc.fromIntegral . the

