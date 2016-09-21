
The Monoid Tutorial
===================

This tutorial is not only educational material for you, but it's also a
playing field for me to explore just how far simple algebraic structures
like monoids can be applied in the context of software design.

[![Creative Commons Attribution-ShareAlike 4.0 International License](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)][cc-by-sa]
This work is licensed under a [Creative Commons Attribution-ShareAlike
4.0 International License][cc-by-sa].

[cc-by-sa]: http://creativecommons.org/licenses/by-sa/4.0/


Semigroups
----------

Let's build up to monoids first; don't worry, it's a relatively short
build-up.  A semigroup is an associative binary function:

```haskell
-- | Associative binary functions.  Values of this type should satisfy
-- the following law:
--
--   * Associativity: for all x, y, z,
--     @(x `sappend` y) `sappend` z = x `sappend` (y `sappend` z)@.

newtype ASemigroup a =
    ASemigroup {
      sappend :: a -> a -> a
    }
```

That's it.  Such a function is commonly referred to as a *composition*
function.  Given two arguments it *composes* them.  Here are a few
examples:

```haskell
-- Compose two integers by addition.
sAdd :: ASemigroup Integer
sAdd = ASemigroup (+)

-- Compose two integers by multiplication.
sMult :: ASemigroup Integer
sMult = ASemigroup (*)

-- Compose two lists by concatenation.
sAppend :: ASemigroup [a]
sAppend = ASemigroup (++)

-- Compose two actions by IO composition.
sAndThen :: ASemigroup (IO a)
sAndThen = ASemigroup (>>)
```

In case you're wondering why I'm not talking about type classes right
now, it's because they're extra, and we'll talk about them later.  It's
better if we understand the essence of the matter first, before we
discuss the convenience features.

It's important to keep in mind that the `ASemigroup` type is tied to a
particular contract: the associativity law.  If we construct a value of
this type that doesn't satisfy it, we are breaking the contract.  Here
is an example of such a violation:

```haskell
sBad :: ASemigroup Integer
sBad = ASemigroup (-)

-- Violation because: (3 - 2) - 1 ≠ 3 - (2 - 1)
```

Back to valid semigroups.  We can of course use any of these semigroups
directly, for which the `-XRecordWildCards` extension is useful,

```haskell
{-# LANGUAGE RecordWildCards #-}

sAddExample :: Integer
sAddExample =
    let ASemigroup{..} = sAdd
    in 3 `sappend` 4 `sappend` 5 `sappend` 6

sAppendExample :: String
sAppendExample =
    let ASemigroup{..} = sAppend
    in foldr sappend [] ["A", "List", "Of", "Words"]
```

but more interestingly we can now abstract over semigroups (take
semigroups as arguments):

```haskell
stimes' :: ASemigroup a -> Integer -> a -> a
stimes' sg@ASemigroup{..} n x =
    case compare n 1 of
      LT -> error "stimes: Non-positive count."
      EQ -> x
      GT -> x `sappend` stimes' sg (n - 1) x
```

This function, given a semigroup, a count `n` and a value `x` composes
`x` with itself the given number of times using the semigroup.
Examples:

```haskell
stimes' add 4 10
  = 10 + (10 + (10 + 10))
  = 40

stimes' append 4 "Hello"
  = "Hello" ++ ("Hello" ++ ("Hello" ++ "Hello"))
  = "HelloHelloHelloHello"

stimes' andThen 4 (putStrLn "blah")
  = putStrLn "blah" >> (putStrLn "blah" >>
    (putStrLn "blah" >> putStrLn "blah"))
```

This seems like a lot of trouble to go through for what is essentially
just passing functions of type `(a -> a -> a)` around, so why is the
associativity law so useful that it warrants its own type?  Actually
associativity is a rather weak law and many functions satisfy it, but
the functions that do tend to be accumulating ("additive") in nature.
Another consequence of associativity is that we can regroup composition
as we wish, so given an expression like,

```haskell
x1 `sappend` (x2 `sappend` (x3 `sappend` x4))
```

we can evaluate the following expression instead and get the same
result:

```haskell
(x1 `sappend` x2) `sappend` (x3 `sappend` x4)
```

This ability to regroup actually gives rise to a few interesting
opportunities to optimise our code, which we will discover later.  For
now let me just give you a particular example.  Integer multiplication
is a semigroup:

```haskell
((((((5 * 5) * 5) * 5) * 5) * 5) * 5) * 5
```

If you were to evaluate this the way it is presented, you would do seven
multiplications.  But since we're allowed to regroup as we wish, we can
compute the following instead,

```haskell
  ((5 * 5) * (5 * 5)) * ((5 * 5) * (5 * 5))
= (25 * 25) * (25 * 25)
= 625 * 625
= 390625
```

which only cost three multiplications.

**Exercise 1**:  Construct a semigroup for the type `()`:

```haskell
exercise1 :: ASemigroup ()
```

**Exercise 2**:  Which of the following are valid semigroups?

```haskell
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
```

**Exercise 3 (hard)**:  Construct interesting semigroups for the
following types:

```haskell
exercise3a :: ASemigroup (a -> a)

exercise3b :: ASemigroup (a -> Integer)

exercise3c :: ASemigroup (a -> IO a)
```


### The Semigroup class

We have talked about the weakness of the associativity law, of which one
consequence is that there are loads and loads of semigroups.  I don't
just mean that in the obvious mathematical sense (there are infinitely
many of them), but that you actually find them all over your code, if
you train your senses to see them.  This also means that as you start
abstracting over semigroups it may get tedious to pass them around as
arguments all the time.  Luckily Haskell has a rather convenient feature
to pass stuff to functions implicitly: type classes.  A class for
semigroups looks like this:

```haskell
-- | Instances of this class should satisfy the following law:
--
--   * Associativity: for all x, y, z,
--     @(x `sappend` y) `sappend` z = x `sappend` (y `sappend` z)@.

class Semigroup a where
    (<>) :: a -> a -> a
```

However, we can save ourselves the trouble of defining this class.
Since base 4.9 (GHC 8.0 and later) it comes as part of the base library.
If your base library is older, well, you should update.  If for some
reason you can't, you can install the [semigroups library] for now.

[semigroups library]: https://hackage.haskell.org/package/semigroups

With this class we can dispense with the (admittedly rather ugly)
`ASemigroup` arguments and record wildcards when abstracting over
semigroups:

```haskell
myStimes :: (Semigroup a) => Integer -> a -> a
myStimes n x =
    case compare n 1 of
      LT -> error "myStimes: Non-positive count"
      EQ -> x
      GT -> x <> myStimes (n - 1) x
```

The reason for the name is, as you may have guessed already, that this
function is actually predefined as `stimes`, except with an optimisation
that relies heavily on the associativity contract, and that for many
semigroups provides an exponential speedup (the square-and-multiply
algorithm).

There is a downside to the class-based approach though: semigroups are
now type-bound: you can only write one semigroup instance for `Integer`.
To overcome this limitation we use `newtype` as usual, and the following
types and instances are actually predefined:

```haskell
newtype Sum a = Sum { getSum :: a }

instance (Num a) => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)

newtype Product a = Product { getProduct :: a }

instance (Num a) => Semigroup (Product a) where
    Product x <> Product y = Product (x * y)
```

When we use `myStimes` we can select the instance based on which wrapper
type we use:

```haskell
myStimes 5 (Sum 3)     = Sum (3 + 3 + 3 + 3 + 3)
myStimes 5 (Product 3) = Product (3 * 3 * 3 * 3 * 3)
```

Even though there are almost always multiple semigroups for any given
type, for some types it's convenient to just declare one of them as
canonical and implement an instance for it.  For example for lists we
use the concatenation semigroup, and we would rarely choose another one,
so we implement:

```haskell
instance Semigroup [a] where
    (<>) = (++)
```
