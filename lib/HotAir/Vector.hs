{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Vector
  ( Vector,
    toList,
    nil,
    cons,
    singleton,
    zipWith,
    take,
    drop,
    append,
    reverse,
    unfoldr
    )
where

import Data.Foldable (Foldable)
import Data.Function (($), (.))
import Data.Functor (Functor)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Semigroup (Semigroup ((<>)))
import Data.Traversable (Traversable)
import GHC.TypeLits (KnownNat, Nat, type (+), type (-))
import GHC.TypeNats (natVal, type (<=))
import HotAir.Bool (ifThenElse)
import HotAir.Eq (Eq ((==)))
import HotAir.List (List)
import qualified HotAir.List as List
import HotAir.Maybe (just, nothing)
import qualified HotAir.Nat as Nat
import HotAir.Pair (Pair, fst, pair, snd)

newtype Vector (n :: Nat) (a :: Type)
  = Vector {toList :: List a}
  deriving (Functor, Foldable, Traversable)

nil :: forall a. Vector 0 a
nil = Vector List.nil

cons
  :: forall (a :: Type) (n :: Nat). a
  -> Vector n a
  -> Vector (1 + n) a
cons a (Vector as) = Vector (a `List.cons` as)

infixr 7 `cons`

singleton :: forall a. a -> Vector 1 a
singleton a = a `cons` nil

zipWith
  :: forall (a :: Type) (b :: Type) (c :: Type) (n :: Nat). (a -> b -> c)
  -> Vector n a
  -> Vector n b
  -> Vector n c
zipWith z (Vector as) (Vector bs) =
  Vector $ List.zipWith z as bs

take
  :: forall (n :: Nat) (m :: Nat) (a :: Type). (KnownNat n, n <= m)
  => Vector m a
  -> Vector n a
take (Vector as) =
  Vector $ List.take (Nat.fromNum (natVal (Proxy @n))) as

drop
  :: forall (n :: Nat) (m :: Nat) (a :: Type). (KnownNat n, n <= m)
  => Vector m a
  -> Vector (m - n) a
drop (Vector as) =
  Vector $ List.drop (Nat.fromNum (natVal (Proxy @n))) as

append
  :: forall (n :: Nat) (m :: Nat) (a :: Type). (KnownNat n, KnownNat m)
  => Vector n a
  -> Vector m a
  -> Vector (n + m) a
append (Vector a) (Vector b) = Vector (a <> b)

reverse :: forall (n :: Nat) a. Vector n a -> Vector n a
reverse = Vector . List.reverse . toList

unfoldr :: forall (n :: Nat) (a :: Type) (b :: Type). KnownNat n => (a -> Pair b a) -> a -> Vector n b
unfoldr f a =
  let n = Nat.fromNum (natVal (Proxy @n))
   in Vector
        $ List.unfoldr
            ( \an ->
                if snd an == Nat.zero
                  then nothing
                  else
                    let ba = f (fst an)
                        b = fst ba
                        a' = snd ba
                     in just (pair b (pair a' (Nat.pred (snd an))))
              )
            (pair a n)
