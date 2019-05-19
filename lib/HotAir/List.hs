{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module HotAir.List
  ( List
  , cons
  , nil
  , foldr
  ) where

import HotAir.Maybe (Maybe, just, nothing)
import HotAir.Pair (Pair, pair)
import HotAir.Semigroup (Semigroup((<>)))

newtype List a =
  List (forall c. c -> (a -> List a -> c) -> c)

nil :: List a
nil = List (\n _ -> n)

cons :: a -> List a -> List a
cons a as = List (\_ c -> c a as)

foldr :: (a -> c -> c) -> c -> List a -> c
foldr f c (List l) = l c (\a as -> f a (foldr f c as))

uncons :: List a -> Maybe (Pair a (List a))
uncons (List l) = l nothing (\a as -> just (pair a as))

instance Semigroup (List a) where
  a <> b = foldr cons b a
