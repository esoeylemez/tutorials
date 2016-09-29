-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

{-# LANGUAGE RecordWildCards #-}

module Monoid where

import Data.Foldable
import Data.Semigroup


-- | Associative binary functions.  Values of this type should satisfy
-- the following law:
--
--   * Associativity: for all x, y, z,
--     @(x `sappend` y) `sappend` z = x `sappend` (y `sappend` z)@.

newtype ASemigroup a =
    ASemigroup {
      sappend :: a -> a -> a
    }

sAdd :: ASemigroup Integer
sAdd = ASemigroup (+)

sMult :: ASemigroup Integer
sMult = ASemigroup (*)

sAppend :: ASemigroup [a]
sAppend = ASemigroup (++)

sAndThen :: ASemigroup (IO a)
sAndThen = ASemigroup (>>)

sBad :: ASemigroup Integer
sBad = ASemigroup (-)

sAddExample :: Integer
sAddExample =
    let ASemigroup{..} = sAdd
    in 3 `sappend` 4 `sappend` 5 `sappend` 6

sAppendExample :: String
sAppendExample =
    let ASemigroup{..} = sAppend
    in foldr sappend [] ["A", "List", "Of", "Words"]

stimes' :: ASemigroup a -> Integer -> a -> a
stimes' sg@ASemigroup{..} n x =
    case compare n 1 of
      LT -> error "stimes': non-positive count"
      EQ -> x
      GT -> x `sappend` stimes' sg (n - 1) x

exercise1 :: ASemigroup ()
exercise1 = ASemigroup (\_ _ -> ())

exercise2a :: ASemigroup (IO a)
exercise2a = ASemigroup (\c1 c2 -> c2 >> c1)

exercise2b :: ASemigroup Rational
exercise2b = ASemigroup (/)

exercise2c :: ASemigroup (Integer, Integer)
exercise2c = ASemigroup (\(x1, x2) (y1, y2) -> (x1 + y2, x2 + y1))

exercise2d :: ASemigroup Integer
exercise2d = ASemigroup max

exercise2e :: ASemigroup Integer
exercise2e = ASemigroup (\x y -> x * (-y))

exercise2f :: ASemigroup [a]
exercise2f = ASemigroup f
    where
    f xs     []     = xs
    f []     ys     = ys
    f (x:xs) (y:ys) = x : y : f xs ys

exercise3a :: ASemigroup (a -> a)
exercise3a = ASemigroup (.)

exercise3b :: ASemigroup (a -> Integer)
exercise3b = ASemigroup (\f g x -> f x + g x)

exercise3c :: ASemigroup (a -> IO a)
exercise3c = ASemigroup (\f g x -> f x >>= g)

myStimes :: (Semigroup a) => Integer -> a -> a
myStimes n x =
    case compare n 1 of
      LT -> error "myStimes: Non-positive count"
      EQ -> x
      GT -> x <> myStimes (n - 1) x

newtype MySum a = MySum { getMySum :: a }

instance (Num a) => Semigroup (MySum a) where
    MySum x <> MySum y = MySum (x + y)

newtype MyProduct a = MyProduct { getMyProduct :: a }

instance (Num a) => Semigroup (MyProduct a) where
    MyProduct x <> MyProduct y = MyProduct (x * y)

listLen :: [a] -> Integer
listLen = foldl' (\c _ -> 1 + c) 0
