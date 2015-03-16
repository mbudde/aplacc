{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module APLAcc.Primitives (
  infinity,
  i2d, b2i,
  signd,
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
  readCharVecFile,
  readVecFile,
  readIntVecFile,
  readDoubleVecFile,
) where


import Control.Monad (liftM, mapM)
import Data.Default
import Prelude hiding (take, drop, reverse, zipWith, sum)
import qualified Data.Array.Accelerate as Acc
import Data.Array.Accelerate (
  Acc, Exp, Elt, Shape, Slice, Plain, Unlift,
  Z(..), (:.)(..), Vector, Scalar, Array )
import Data.Array.Accelerate.Array.Sugar (shapeToList)


instance (Default a, Elt a) => Default (Exp a) where
  def = Acc.constant def

instance Default Bool where
  def = False

instance Default Char where
  def = ' '

-- A type-level function that gives the unlifted type of an (Exp shape)
class Unlifting e where
  type Unlifted e

instance Unlifting Z where
  type Unlifted Z = Z

instance Unlifting sh => Unlifting (sh :. Int) where
  type Unlifted (sh :. Int) = Unlifted sh :. Exp Int

-- A typeclass for shapes that can be indexed
class IndexShape sh where
  indexSh :: sh -> Exp Int -> Exp Int
  dimSh   :: sh -> Exp Int

instance IndexShape (Exp Z) where
  indexSh _ _ = Acc.constant 0
  dimSh _ = Acc.constant 0

instance (Slice sh, IndexShape (Exp sh)) => IndexShape (Exp (sh :. Int)) where
  indexSh e n = Acc.cond (n Acc.==* 0) (Acc.indexHead e)
                                       (indexSh (Acc.indexTail e) (n-1))
  dimSh e = 1 + dimSh (Acc.indexTail e)

-- A typeclass for shapes that can be reordered using another shape as list of indices.
class (IndexShape shOrder) => Exchange shOrder where
  exchange :: IndexShape sh => shOrder -> sh -> shOrder

instance Exchange (Exp Z) where
  exchange _ _ = Acc.constant Z

instance (sh ~ Plain (Unlifted sh),
          Slice sh,
          IndexShape (Exp sh),
          Exchange (Exp sh),
          (Unlift Exp (Unlifted sh))) => Exchange (Exp (sh :. Int)) where
  exchange order sh = Acc.lift $ r :. indexSh sh (Acc.indexHead order - 1)
    where
      r :: Unlifted sh
      r = Acc.unlift $ exchange (Acc.indexTail order) sh

-- A typeclass for generating shapes of the form (Z :. 1 :. 2 :. ...)
class (Shape sh) => Iota sh where
  iotaSh :: Exp sh
  iotaNext :: Exp sh -> Exp Int

instance Iota Z where
  iotaSh = Acc.constant Z
  iotaNext _ = Acc.constant 1

instance (Iota sh, Slice sh) => Iota (sh :. Int) where
  iotaSh = Acc.lift $ sh :. iotaNext sh
    where sh = iotaSh

  iotaNext sh = iotaNext (Acc.indexTail sh) + 1


infinity :: Double
infinity = 1/0

i2d :: (Elt a, Elt b, Acc.IsIntegral a, Acc.IsNum b)
    => Exp a -> Exp b
i2d = Acc.fromIntegral

b2i :: Exp Bool -> Exp Int
b2i = Acc.boolToInt

signd :: Exp Double -> Exp Int
signd = Acc.truncate . signum

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

vrotate :: (Elt e, Shape sh, Slice sh, Iota (sh :. Int), Exchange (Exp (sh :. Int)))
        => Exp Int
        -> Acc (Array (sh :. Int) e)
        -> Acc (Array (sh :. Int) e)
vrotate n = transp . rotate n . transp

transp :: (Elt e, Iota sh, Exchange (Exp sh)) => Acc (Array sh e) -> Acc (Array sh e)
transp = let sh = iotaSh in transp2 (sh, sh)

transp2 :: (Exchange (Exp ix), Elt a, Shape ix) =>  (Exp ix, Exp ix) -> Acc (Array ix a) -> Acc (Array ix a)
transp2 (order, orderInv) array = Acc.backpermute newShape reorder array
  where
    newShape = exchange order (Acc.shape array)
    reorder = exchange orderInv

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
     :: (Shape sh, Slice sh, Iota (sh :. Int), Exchange (Exp (sh :. Int)), Elt a, Default a)
     => Exp Int
     -> Acc (Array (sh :. Int) a)
     -> Acc (Array (sh :. Int) a)

takeAux n arr =
  let sh' = Acc.lift $ Acc.indexTail (Acc.shape arr) :. n
  in  Acc.backpermute sh' id arr

take n arr =
  let arr' = transp arr
      sh = Acc.shape arr' in
  abs n Acc.>* Acc.indexHead sh Acc.?|
    ( transp $ padArray n arr'
    , n Acc.>=* 0 Acc.?|
        ( transp $ takeAux n arr'
        , transp $ dropAux (Acc.indexHead sh + n) arr' ))

dropAux n arr =
  let sh = Acc.shape arr
      sh' = Acc.lift $ Acc.indexTail sh :. max 0 (Acc.indexHead sh - n)
      idx sh = Acc.lift $ Acc.indexTail sh :. Acc.indexHead sh + n
  in  Acc.backpermute sh' idx arr

drop n arr =
  let arr' = transp arr
      sh = Acc.shape arr'
      m = min (Acc.indexHead sh) (abs n) in
  n Acc.>=* 0 Acc.?|
    ( transp $ dropAux m arr'
    , transp $ takeAux (Acc.indexHead sh - m) arr' )

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


-- Input/output primitives

readCharVecFile :: String -> IO (Acc (Vector Char))
readCharVecFile file =
  do contents <- Prelude.readFile file
     return $ Acc.use $ Acc.fromList (Z :. (length contents) :: Acc.DIM1) contents

readVecFile :: (Read a, Elt a) => String -> IO (Acc (Vector a))
readVecFile file =
  do contents <- Prelude.readFile file
     let values =  map read $ lines contents
     return $ Acc.use $ Acc.fromList (Z :. (length values) :: Acc.DIM1) values

readIntVecFile :: String -> IO (Acc (Vector Int))
readIntVecFile = readVecFile
readDoubleVecFile :: String -> IO (Acc (Vector Double))
readDoubleVecFile = readVecFile
