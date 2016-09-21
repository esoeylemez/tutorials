
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
add :: ASemigroup Integer
add = ASemigroup (+)

-- Compose two integers by multiplication.
mult :: ASemigroup Integer
mult = ASemigroup (*)

-- Compose two lists by concatenation.
append :: ASemigroup [a]
append = ASemigroup (++)

-- Compose two actions by IO composition.
andThen :: ASemigroup (IO a)
andThen = ASemigroup (>>)
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
badSemigroup :: ASemigroup Integer
badSemigroup = ASemigroup (-)

-- Violation because: (3 - 2) - 1 ≠ 3 - (2 - 1)
```

Back to valid semigroups.  We can of course use any of these semigroups
directly, for which the `-XRecordWildCards` extension is useful,

```haskell
{-# LANGUAGE RecordWildCards #-}

addExample :: Integer
addExample =
    let ASemigroup{..} = add
    in 3 `sappend` 4 `sappend` 5 `sappend` 6

appendExample :: String
appendExample =
    let ASemigroup{..} = append
    in foldr sappend [] ["A", "List", "Of", "Words"]
```

but more interestingly we can now abstract over semigroups (take
semigroups as arguments):

```haskell
stimes :: ASemigroup a -> Integer -> a -> a
stimes sg@ASemigroup{..} n x =
    case compare n 1 of
      LT -> error "stimes: Non-positive count."
      EQ -> x
      GT -> x `sappend` stimes sg (n - 1) x
```

This function, given a semigroup, a count `n` and a value `x` composes
`x` with itself the given number of times using the semigroup.
Examples:

```haskell
stimes add 4 10
  = 10 + (10 + (10 + 10))
  = 40

stimes append 4 "Hello"
  = "Hello" ++ ("Hello" ++ ("Hello" ++ "Hello"))
  = "HelloHelloHelloHello"

stimes andThen 4 (putStrLn "blah")
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
