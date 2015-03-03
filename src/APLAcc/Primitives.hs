{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module APLAcc.Primitives (
  infinity,
  i2d, b2i,
  residue,
  zilde,
  iota, iotaV,
  unitvec,
  each, eachV,
  reduce,
  shape, shapeV,
  power,
  reshape0, reshape,
  reverse,
  rotate, rotateV,
  vrotate,
  transp, transp2,
  take, takeV,
  drop, dropV,
  first, firstV,
  zipWith,
  cat, catV,
  cons, consV,
  snoc, snocV,
  sum,
) where


import Data.Default
import Prelude hiding (take, drop, reverse, zipWith, sum)
import qualified Data.Array.Accelerate as Acc
import Data.Array.Accelerate (Acc, Exp, Elt, Shape, Slice,
                              Z(..), (:.)(..), Vector, Scalar, Array)
import Data.Array.Accelerate.Array.Sugar (shapeToList)

instance (Default a, Elt a) => Default (Exp a) where
  def = Acc.constant def

instance Default Bool where
  def = False

instance Default Char where
  def = ' '

infinity :: Double
infinity = 1/0

i2d :: (Elt a, Elt b, Acc.IsIntegral a, Acc.IsNum b)
    => Exp a -> Exp b
i2d = Acc.fromIntegral

b2i :: (Elt b, Acc.IsIntegral b) => Exp Bool -> Exp b
b2i b = (Acc.?) b (1, 0)

residue :: (Elt a, Acc.IsIntegral a) => Exp a -> Exp a -> Exp a
residue a b = Acc.cond (a Acc.==* 0) b (b `mod` a)

zilde :: Elt e => Acc (Vector e)
zilde = Acc.use (Acc.fromList (Z :. 0) [])

iota :: (Elt e, Acc.IsNum e) => Exp Int -> Acc (Vector e)
iota n = Acc.enumFromN (Acc.index1 n) 1

iotaV :: Exp Int -> Acc (Vector Int)
iotaV = iota

unitvec :: (Elt e) => Acc (Scalar e) -> Acc (Vector e)
unitvec = Acc.reshape (Acc.lift $ Z :. (1 :: Int))

each :: (Shape ix, Elt a, Elt b)
     => (Exp a -> Exp b)
     -> Acc (Array ix a)
     -> Acc (Array ix b)
each = Acc.map

eachV :: (Elt a, Elt b)
      => (Exp a -> Exp b)
      -> Acc (Vector a)
      -> Acc (Vector b)
eachV = each

reduce :: (Shape ix, Elt a)
       => (Exp a -> Exp a -> Exp a)
       -> Exp a
       -> Acc (Array (ix :. Int) a)
       -> Acc (Array ix a)
reduce = Acc.fold

power :: forall a. (Acc.Arrays a)
      => (Acc a -> Acc a)
      -> Exp Int
      -> Acc a
      -> Acc a
power fn n arr = unpack snd $
  Acc.awhile (unpack (\(m, _) -> Acc.unit $ Acc.the m Acc.>* 0))
             (unpack (\(m, a) -> Acc.lift $ (each (\k -> k-1) m, fn a)))
             (Acc.lift (Acc.unit n, arr))
  where unpack :: (Acc.Arrays b) => ((Acc (Scalar Int), Acc a) -> Acc b) -> Acc (Scalar Int, a) -> Acc b
        unpack f x = let y = Acc.unlift x in f y

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

shapeV :: (Elt e) => Acc (Vector e) -> Exp Int
shapeV arr = shape arr Acc.!! 0

reshape0 :: (Shape ix, Shape ix', Elt e)
         => Exp ix
         -> Acc (Array ix' e)
         -> Acc (Array ix e)
reshape0 sh arr = Acc.backpermute sh (Acc.fromIndex sh' . flip mod m . Acc.toIndex sh) arr
  where m   = Acc.size arr
        sh' = Acc.shape arr

reshape :: (Shape ix, Shape ix', Elt e)
        => Exp ix
        -> Exp e
        -> Acc (Array ix' e)
        -> Acc (Array ix e)
reshape sh el arr = Acc.acond (Acc.null arr) (Acc.fill sh el) (reshape0 sh arr)


reverse :: (Shape sh, Slice sh, Elt e)
        => Acc (Array (sh :. Int) e)
        -> Acc (Array (sh :. Int) e)
reverse arr = Acc.backpermute (Acc.shape arr) idx arr
  where m = Acc.indexHead $ Acc.shape arr
        idx sh = Acc.lift $ Acc.indexTail sh :. m - Acc.indexHead sh - 1


rotate :: (Shape sh, Slice sh, Elt e)
       => Exp Int -> Acc (Array (sh :. Int) e) -> Acc (Array (sh :. Int) e)
rotate n arr =
  let sh = Acc.shape arr
      m  = Acc.indexHead sh
      idx sh = Acc.lift $ Acc.indexTail sh :. (Acc.indexHead sh + n) `mod` m
  in Acc.backpermute sh idx arr

rotateV :: (Elt e) => Exp Int -> Acc (Vector e) -> Acc (Vector e)
rotateV = rotate

vrotate :: (Elt e) => Exp Int -> Acc (Array Acc.DIM2 e) -> Acc (Array Acc.DIM2 e)
vrotate n = transp . rotate n . transp

transp :: (Elt e) => Acc (Array Acc.DIM2 e) -> Acc (Array Acc.DIM2 e)
transp = Acc.transpose

transp2 :: (Shape sh, Elt e) => Acc (Vector Int) -> Acc (Array sh e) -> Acc (Array sh e)
transp2 dimIdx arr = undefined

padArray :: (Slice sh, Shape sh, Elt a, Default a)
         => Exp Int
         -> Acc (Array (sh :. Int) a)
         -> Acc (Array (sh :. Int) a)
padArray n arr =
  let sh = Acc.shape arr
      sh' = Acc.lift $ Acc.indexTail sh :. (abs n) - Acc.indexHead sh
      zeroes = Acc.generate sh' (\_ -> def)
  in n Acc.>=* 0 Acc.?| (arr Acc.++ zeroes, zeroes Acc.++ arr)

take, takeAux, drop, dropAux, takeV, dropV
     :: (Slice sh, Shape sh, Elt a, Default a)
     => Exp Int
     -> Acc (Array (sh :. Int) a)
     -> Acc (Array (sh :. Int) a)

-- FIXME: operate on first dimension instead of last
takeAux n arr =
  let sh' = Acc.lift $ Acc.indexTail (Acc.shape arr) :. n in
  Acc.backpermute sh' id arr

take n arr =
  let sh = Acc.shape arr in
  abs n Acc.>* Acc.indexHead sh Acc.?|
    ( padArray n arr
    , n Acc.>=* 0 Acc.?|
        ( takeAux n arr
        , dropAux (Acc.indexHead sh + n) arr ))

-- FIXME: operate on first dimension instead of last
dropAux n arr =
  let sh  = Acc.shape arr
      sh' = Acc.lift $ Acc.indexTail sh :. max 0 (Acc.indexHead sh - n)
      idx sh = Acc.lift $ Acc.indexTail sh :. Acc.indexHead sh + n
  in Acc.backpermute sh' idx arr

drop n arr =
  let sh = Acc.shape arr
      m = min (Acc.indexHead sh) (abs n) in
  n Acc.>=* 0 Acc.?|
    ( dropAux m arr
    , takeAux (Acc.indexHead sh - m) arr )

takeV = take
dropV = drop

first :: (Shape sh, Elt e, Acc.IsNum e) => Acc (Array sh e) -> Exp e
first arr = Acc.cond (Acc.null arr) 0 (arr Acc.!! 0)

firstV :: (Elt e, Acc.IsNum e) => Acc (Vector e) -> Exp e
firstV = first

zipWith :: (Shape sh, Elt a, Elt b, Elt c)
        => (Exp a -> Exp b -> Exp c)
        -> Acc (Array sh a)
        -> Acc (Array sh b)
        -> Acc (Array sh c)
zipWith = Acc.zipWith

cat, catV :: forall sh e. (Slice sh, Shape sh, Elt e)
    => Acc (Array (sh :. Int) e)
    -> Acc (Array (sh :. Int) e)
    -> Acc (Array (sh :. Int) e)
cat = (Acc.++)
catV = cat

extend :: (Shape sh, Slice sh, Elt e) => Acc (Array sh e) -> Acc (Array (sh :. Int) e)
extend arr =
  let sh  = Acc.shape arr
      sh' = Acc.lift $ sh :. (1 :: Int)
      idx = Acc.lift . Acc.indexTail
  in Acc.backpermute sh' idx arr

cons :: (Shape sh, Slice sh, Elt e)
     => Acc (Array sh e)
     -> Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
cons a1 a2 = extend a1 Acc.++ a2

consV :: (Elt e) => Exp e -> Acc (Vector e) -> Acc (Vector e)
consV e = cons (Acc.unit e)

snoc :: (Shape sh, Slice sh, Elt e)
     => Acc (Array (sh :. Int) e)
     -> Acc (Array sh e)
     -> Acc (Array (sh :. Int) e)
snoc a1 a2 = a1 Acc.++ extend a2

snocV :: (Elt e) => Acc (Vector e) -> Exp e -> Acc (Vector e)
snocV arr e = snoc arr (Acc.unit e)

sum :: (Elt e, Acc.IsNum e)
    => (Exp e -> Exp e -> Exp e)
    -> Acc (Vector e)
    -> Acc (Vector e)
    -> Acc (Vector e)
sum g a b =
  let sh1 = Acc.unindex1 $ Acc.shape a
      sh2 = Acc.unindex1 $ Acc.shape b
  in if sh1 == sh2 then Acc.zipWith (+) a b else zilde
