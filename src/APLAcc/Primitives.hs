{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module APLAcc.Primitives (
  i2d,
  zilde,
  iota, iotaSh,
  unitvec,
  each,
  reduce,
  shape, shapeSh,
  reshape0, reshape,
  reverse,
  rotate, rotateSh,
  transp, transp2,
  take, takeSh,
  drop, dropSh,
  first, firstSh,
  zipWith,
  cat, catSh,
  cons, consSh,
  snoc, snocSh,
  sum,
) where


import Prelude hiding (take, drop, reverse, zipWith, sum)
import qualified Data.Array.Accelerate as Acc
import Data.Array.Accelerate (Acc, Exp, Elt, Shape, Slice,
                              Z(..), (:.)(..), Vector, Scalar, Array)
import Data.Array.Accelerate.Array.Sugar (shapeToList)


i2d :: (Elt a, Elt b, Acc.IsIntegral a, Acc.IsNum b)
    => Exp a -> Exp b
i2d = Acc.fromIntegral

zilde :: Elt e => Acc (Vector e)
zilde = Acc.use (Acc.fromList (Z :. 0) [])

iota, iotaSh :: Int -> Acc (Vector Int)
iota n = Acc.use $ Acc.fromList (Z :. n) [1..]
iotaSh = iota

unitvec :: (Elt e) => Acc (Scalar e) -> Acc (Vector e)
unitvec = Acc.reshape (Acc.lift $ Z :. (1 :: Int))

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

class IndexShape sh where
  indexSh :: sh -> Exp Int -> Exp Int
  dimSh   :: sh -> Exp Int

instance IndexShape (Exp Z) where
  indexSh _ _ = Acc.constant 0
  dimSh _ = Acc.constant 0

instance (Acc.Slice sh, IndexShape (Exp sh)) => IndexShape (Exp (sh :. Int)) where
  indexSh e n = Acc.cond (n Acc.==* 0) (Acc.indexHead e)
                                       (indexSh (Acc.indexTail e) (n-1))
  dimSh e = 1 + dimSh (Acc.indexTail e)


shape :: (Shape sh, Elt e, IndexShape (Exp sh))
      => Acc (Array sh e) -> Acc (Vector Int)
shape arr =
  let sh = Acc.shape arr
  in Acc.generate (Acc.lift $ Z :. dimSh sh) (indexSh sh . Acc.indexHead)

shapeSh :: Acc (Vector Int) -> Exp Int
shapeSh arr = shape arr Acc.!! 0

reshape0 = undefined

reshape :: (Shape ix, Shape ix', Elt e)
        => Exp ix
        -> Acc (Array ix' e)
        -> Acc (Array ix e)
reshape sh arr = Acc.backpermute sh (Acc.fromIndex sh' . flip mod m . Acc.toIndex sh) arr
  where m   = Acc.size arr
        sh' = Acc.shape arr

reverse = undefined


rotate :: (Shape sh, Slice sh, Elt e)
       => Exp Int -> Acc (Array (sh :. Int) e) -> Acc (Array (sh :. Int) e)
rotate n arr =
  let sh = Acc.shape arr
      m  = Acc.indexHead sh
      idx sh = Acc.lift $ Acc.indexTail sh :. (Acc.indexHead sh + n) `mod` m
  in Acc.backpermute sh idx arr

rotateSh :: (Elt e) => Exp Int -> Acc (Vector e) -> Acc (Vector e)
rotateSh = rotate

transp :: (Elt e) => Acc (Array Acc.DIM2 e) -> Acc (Array Acc.DIM2 e)
transp = Acc.transpose

transp2 :: (Shape sh, Elt e) => Acc (Vector Int) -> Acc (Array sh e) -> Acc (Array sh e)
transp2 dimIdx arr = undefined

take, drop, takeSh, dropSh
     :: (Slice sh, Shape sh, Elt a)
     => Exp Int
     -> Acc (Array (sh :. Int) a)
     -> Acc (Array (sh :. Int) a)
take n arr =
  let sh' = Acc.lift $ Acc.indexTail (Acc.shape arr) :. n
  in Acc.backpermute sh' id arr
takeSh = take

drop n arr =
  let sh  = Acc.shape arr
      sh' = Acc.lift $ Acc.indexTail sh :. Acc.indexHead sh - n
      idx sh = Acc.lift $ Acc.indexTail sh :. Acc.indexHead sh + n
  in Acc.backpermute sh' idx arr
dropSh = drop

first :: (Shape sh, Elt e) => Acc (Array sh e) -> Exp e
first arr = arr Acc.!! 0

firstSh :: Acc (Vector Int) -> Exp Int
firstSh = first

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
consSh e arr = Acc.use (Acc.fromList (Z :. 1) [e]) Acc.++ arr

snoc = undefined
snocSh = snoc

sum :: (Elt e, Acc.IsNum e)
    => (Exp e -> Exp e -> Exp e)
    -> Acc (Vector e)
    -> Acc (Vector e)
    -> Acc (Vector e)
sum g a b =
  let sh1 = Acc.unindex1 $ Acc.shape a
      sh2 = Acc.unindex1 $ Acc.shape b
  in if sh1 == sh2 then Acc.zipWith (+) a b else zilde
