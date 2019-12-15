{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Nat
  ( Nat,
    zero,
    succ,
    nat,
    pred,
    fromNum,
    toNum
    )
where

import Data.Function ((.))
import GHC.Num (fromInteger)
import qualified GHC.Num as Builtin
import HotAir.Bool (Bool, false, ifThenElse, true)
import HotAir.Eq (Eq ((==)))
import HotAir.Maybe (Maybe, just, maybe, nothing)
import HotAir.Ord (Ord ((<=)))

newtype Nat
  = Nat (forall c. c -> (Nat -> c) -> c)

zero :: Nat
zero = Nat \z _ -> z

succ :: Nat -> Nat
succ n = Nat \_ s -> s n

nat :: c -> (Nat -> c) -> Nat -> c
nat z s (Nat n) = n z s

pred :: Nat -> Nat
pred = nat zero \n -> n

foldNat :: c -> (c -> c) -> Nat -> c
foldNat z f = nat z (f . foldNat z f)

toNum :: Builtin.Num n => Nat -> n
toNum = foldNat 0 (Builtin.+ 1)

unfoldNat :: (c -> Maybe c) -> c -> Nat
unfoldNat f c = maybe zero (succ . unfoldNat f) (f c)

fromNum :: (Ord n, Builtin.Num n) => n -> Nat
fromNum =
  unfoldNat \n ->
    if n <= 0
      then nothing
      else just (n Builtin.- 1)

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  a == b =
    nat (nat true (\_ -> false) a) (pred a ==) b
