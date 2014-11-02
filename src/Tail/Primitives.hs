{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Tail.Primitives where

import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Array.Sugar (shapeToList)


{- i2d :: (Elt a, Elt b, IsIntegral a, IsNum b) -}
    {- => Acc (Scalar a) -> Acc (Scalar b) -}
{- i2d = unit . Acc.fromIntegral . the -}

i2d :: (Elt a, Elt b, IsIntegral a, IsNum b)
    => Exp a -> Exp b
i2d = Acc.fromIntegral

zilde :: Elt e => Acc (Vector e)
zilde = Acc.use (Acc.fromList (Z :. 0) [])

iota, iotaSh :: Int -> Acc (Vector Int)
iota n = Acc.use $ Acc.fromList (Z :. n) [1..]
iotaSh = iota

each :: (Shape ix, Elt a, Elt b)
     => (Exp a -> Exp b)
     -> Acc (Array ix a)
     -> Acc (Array ix b)
each = Acc.map

reduce :: (Shape ix, Elt a)
       => (Exp a -> Exp a -> Exp a)
       -> Exp a
       -> Acc (Array (ix :. Int) a)
       -> Acc (Array ix a)
reduce = Acc.fold

{- shape = undefined -}
{- shapeSh = undefined -}
{- shape :: forall ix e. (Shape ix, Elt e) => Acc (Array ix e) -> Acc (Vector Int) -}
{- shape arr = -}
  {- let shapeList = shapeToList $ (unlift $ Acc.shape arr :: ix) -}
  {- in Acc.use $ Acc.fromList (Z :. length shapeList) shapeList -}

class IndexShape sh where
  indexSh :: sh -> Exp Int -> Exp Int
  dimSh   :: sh -> Exp Int

instance IndexShape (Exp Z) where
  indexSh _ _ = constant 0
  dimSh _ = constant 0

instance (Slice sh, IndexShape (Exp sh)) => IndexShape (Exp (sh :. Int)) where
  indexSh e n = cond (n ==* 0) (indexHead e)
                               (indexSh (indexTail e) (n-1))
  dimSh e = 1 + dimSh (indexTail e)


shape, shapeSh :: (Shape sh, Elt e, IndexShape (Exp sh))
               => Acc (Array sh e) -> Acc (Vector Int)
shape arr =
  let sh = Acc.shape arr
  in Acc.generate (lift $ Z :. (dimSh sh)) (indexSh sh . indexHead)
shapeSh = Tail.Primitives.shape

reshape0 = undefined

reshape :: (Shape ix, Shape ix', Elt e)
        => Exp ix
        -> Acc (Array ix' e)
        -> Acc (Array ix e)
reshape = Acc.reshape

reverse = undefined


rotate :: (Shape sh, Slice sh, Elt e)
       => Exp Int -> Acc (Array (sh :. Int) e) -> Acc (Array (sh :. Int) e)
rotate n arr =
  let sh = Acc.shape arr
      m = Acc.indexHead sh
      idx sh = lift $ Acc.indexTail sh :. (Acc.indexHead sh + n) `mod` m
  in backpermute sh idx arr

rotateSh :: (Elt e) => Exp Int -> Acc (Vector e) -> Acc (Vector e)
rotateSh = Tail.Primitives.rotate

transp :: (Elt e) => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
transp = Acc.transpose

transp2 = undefined

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

first :: (Shape sh, Elt e) => Acc (Array sh e) -> Exp e
first arr = arr Acc.!! 0

firstSh :: Acc (Vector Int) -> Exp Int
firstSh = Tail.Primitives.first

zipWith :: (Shape sh, Elt a, Elt b, Elt c)
        => (Exp a -> Exp b -> Exp c)
        -> Acc (Array sh a)
        -> Acc (Array sh b)
        -> Acc (Array sh c)
zipWith = Acc.zipWith

cat, catSh :: forall sh e. (Slice sh, Shape sh, Elt e)
    => Acc (Array (sh :. Int) e)
    -> Acc (Array (sh :. Int) e)
    -> Acc (Array (sh :. Int) e)
cat = (Acc.++)
catSh = cat

cons :: (Shape sh, Elt e)
     => Acc (Array sh e)
     -> Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
cons = undefined

consSh :: (Elt e) => e -> Acc (Vector e) -> Acc (Vector e)
consSh e arr = Acc.use (fromList (Z :. 1) [e]) Acc.++ arr

{- snoc :: (Elt e, Shape sh)
     => Acc (Array (sh :. Int) e)
     -> Acc (Array  sh         e)
     -> Acc (Array (sh :. Int) e) -}
{- snoc :: forall sh e. (Elt e, Shape sh)
     => Acc (Vector e)
     -> Acc (Scalar e)
     -> Acc (Vector e)
snoc xs ys
  = let sh1 :. n      = unlift (shape xs)      :: Exp sh :. Exp Int
        sh2           = shape ys               :: Exp sh
    in
    generate (lift (sh1)) -}
snoc = undefined
snocSh = snoc

sum :: (Elt e, IsNum e)
    => (Exp e -> Exp e -> Exp e)
    -> Acc (Vector e)
    -> Acc (Vector e)
    -> Acc (Vector e)
sum g a b =
  let sh1 = Acc.unindex1 $ Acc.shape a
      sh2 = Acc.unindex1 $ Acc.shape b
  in if sh1 == sh2 then Acc.zipWith (+) a b else zilde
