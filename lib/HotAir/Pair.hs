{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Pair
  ( Pair,
    pair,
    fst,
    snd
    )
where

newtype Pair a b
  = Pair (forall c. (a -> b -> c) -> c)

pair :: a -> b -> Pair a b
pair a b = Pair \p -> p a b

fst :: Pair a b -> a
fst (Pair p) = p \a _ -> a

snd :: Pair a b -> b
snd (Pair p) = p \_ b -> b
