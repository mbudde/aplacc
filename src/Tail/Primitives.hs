{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}

module Tail.Primitives where

import Data.Array.Accelerate as Acc


iota, iotaSh :: Int -> Acc (Vector Int)
iota n = Acc.use $ Acc.fromList (Z :. n) [1..]
iotaSh = iota


i2d :: (Elt a, Elt b, IsIntegral a, IsNum b)
    => Acc (Scalar a) -> Acc (Scalar b)
i2d = unit . Acc.fromIntegral . the


reduce :: (Shape ix, Elt a)
       => (Exp a -> Exp a -> Exp a)
       -> Exp a
       -> Acc (Array (ix :. Int) a)
       -> Acc (Array ix a)
reduce = Acc.fold


each :: (Shape ix, Elt a, Elt b)
     => (Exp a -> Exp b)
     -> Acc (Array ix a)
     -> Acc (Array ix b)
each = Acc.map


take, drop, takeSh, dropSh
     :: (Slice sh, Shape sh, Elt a)
     => Exp Int
     -> Acc (Array (sh :. Int) a)
     -> Acc (Array (sh :. Int) a)
take n arr =
  let sh' = lift $ Acc.indexTail (Acc.shape arr) :. n
  in backpermute sh' id arr
takeSh = Tail.Primitives.take

drop n arr =
  let sh = Acc.shape arr
      sh' = lift $ Acc.indexTail sh :. Acc.indexHead sh - n
      idx sh = lift $ Acc.indexTail sh :. Acc.indexHead sh + n
  in backpermute sh' idx arr
dropSh = Tail.Primitives.drop


sum :: (Elt e, IsNum e)
    => (Exp e -> Exp e -> Exp e)
    -> Acc (Vector e)
    -> Acc (Vector e)
    -> Acc (Vector e)
sum g a b =
  let sh1 = Acc.unindex1 $ Acc.shape a
      sh2 = Acc.unindex1 $ Acc.shape b
  in if sh1 == sh2 then Acc.zipWith (+) a b else zilde


add :: (Elt e, IsNum e)
    => Acc (Scalar e) -> Acc (Scalar e) -> Acc (Scalar e)
add a b = Acc.unit (Acc.the a + Acc.the b)


zilde :: Elt e => Acc (Vector e)
zilde = Acc.use (Acc.fromList (Z :. 0) [])


cat, catSh :: forall sh e. (Slice sh, Shape sh, Elt e)
    => Acc (Array (sh :. Int) e)
    -> Acc (Array (sh :. Int) e)
    -> Acc (Array (sh :. Int) e)
cat = (Acc.++)
catSh = cat

reshape :: (Shape ix, Shape ix', Elt e)
        => Exp ix
        -> Acc (Array ix' e)
        -> Acc (Array ix e)
reshape = Acc.reshape
