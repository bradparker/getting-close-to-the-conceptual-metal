{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Nat
  ( Nat,
    zero,
    succ,
    divMod,
    foldNat,
    fromIntegral,
    toNum
    )
where

import Data.Function (($), (.), flip, id)
import Data.Monoid (Monoid (mempty))
import Data.Semigroup (Semigroup ((<>)), stimesMonoid)
import GHC.Err (error)
import GHC.Integer (Integer)
import GHC.Num (Num (..))
import GHC.Real (Integral)
import HotAir.Bool (Bool, false, ifThenElse, true)
import HotAir.Eq (Eq ((==)))
import HotAir.Pair (Pair, pair)

newtype Nat
  = Nat (forall c. c -> (Nat -> c) -> c)

zero :: Nat
zero = Nat $ \z _ -> z

succ :: Nat -> Nat
succ n = Nat $ \_ s -> s n

nat :: c -> (Nat -> c) -> Nat -> c
nat z s (Nat n) = n z s

pred :: Nat -> Nat
pred = nat zero $ \n -> n

foldNat :: c -> (c -> c) -> Nat -> c
foldNat z f = nat z (f . foldNat z f)

toNum :: Num n => Nat -> n
toNum = foldNat 0 (+ 1)

newtype Add = Add {getAdd :: Nat}

instance Semigroup Add where
  (<>) :: Add -> Add -> Add
  Add a <> Add b = Add (a + b)

instance Monoid Add where
  mempty :: Add
  mempty = Add zero

fromIntegral :: Integral n => n -> Nat
fromIntegral = getAdd . flip stimesMonoid (Add (succ zero))

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  a == b =
    nat
      ( nat
          true
          (\_ -> false)
          a
        )
      ( \b' ->
          nat
            false
            (\a' -> a' == b')
            a
        )
      b

instance Num Nat where

  (+) :: Nat -> Nat -> Nat
  (+) a = foldNat a succ

  (*) :: Nat -> Nat -> Nat
  (*) a = foldNat zero (+ a)

  (-) :: Nat -> Nat -> Nat
  (-) a = foldNat a pred

  signum :: Nat -> Nat
  signum n = if n == zero then zero else succ zero

  negate :: Nat -> Nat
  negate = error "There are no negative Naturals"

  fromInteger :: Integer -> Nat
  fromInteger = fromIntegral

  abs :: Nat -> Nat
  abs = id

divMod :: Nat -> Nat -> Pair Nat Nat
divMod n d = go 0 n
  where
    go :: Nat -> Nat -> Pair Nat Nat
    go c n' =
      let { n'' = n' - d }
       in if n' == d
            then pair (c + 1) 0
            else
              if n'' == 0
                then pair c n'
                else go (c + 1) n''
