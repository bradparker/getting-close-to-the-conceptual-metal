{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Pair
  ( Pair,
    pair,
    fst,
    snd,
    both
    )
where

import Control.Applicative (Applicative ((<*>), pure))
import Data.Function (($))
import Data.Functor (Functor)
import Data.Monoid (Monoid (mempty))
import Data.Semigroup ((<>))

newtype Pair a b
  = Pair (forall c. (a -> b -> c) -> c)
  deriving (Functor)

pair :: a -> b -> Pair a b
pair a b = Pair $ \p -> p a b

fst :: Pair a b -> a
fst (Pair p) = p $ \a _ -> a

snd :: Pair a b -> b
snd (Pair p) = p $ \_ b -> b

both :: (a -> b -> c) -> Pair a b -> c
both f (Pair p) = p f

instance Monoid m => Applicative (Pair m) where

  pure :: a -> Pair m a
  pure = pair mempty

  (<*>) :: Pair m (a -> b) -> Pair m a -> Pair m b
  a <*> b = pair (fst a <> fst b) (snd a (snd b))
