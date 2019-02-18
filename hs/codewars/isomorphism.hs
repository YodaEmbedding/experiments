-- https://www.codewars.com/kata/isomorphism/haskell

module ISO where

import Data.Maybe
import Data.Tuple
import Data.Void

type ISO a b = (a -> b, b -> a)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

refl :: ISO a a
refl = (id, id)

symm :: ISO a b -> ISO b a
symm = swap

trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)

isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = (iso ab cd, iso ba dc) where
  iso f g (x, y) = (f x, g y)

isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (map ab, map ba)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (fmap ab, fmap ba)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (iso ab cd, iso ba dc) where
  iso xy yx (Left x)  = Left (xy x)
  iso xy yx (Right y) = Right (yx y)

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = (iso ba cd, iso ab dc) where
  iso f h g = h . g . f

isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (mamb, mbma) = (iso mamb, iso mbma) where
  iso f x = fromMaybe (fromJust $ f Nothing) (f $ Just x)

isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (ab, ba) where
  ab (Left xs) = Left (():xs)
  ab (Right x) = Left []
  ba (Left (x:xs)) = Left xs
  ba (Left []) = Right ()
  ba (Right x) = Right (absurd x)

isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (swap, swap)
