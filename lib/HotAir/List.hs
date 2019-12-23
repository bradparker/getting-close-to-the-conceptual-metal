{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.List
  ( List,
    cons,
    nil,
    foldr,
    uncons,
    singleton,
    reverse
    )
where

import Control.Applicative (Applicative ((<*>), pure))
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Function (flip)
import Data.Functor ((<$>), Functor (fmap))
import Data.Semigroup (Semigroup ((<>)))
import Data.Traversable (Traversable (traverse))
import GHC.Exts (IsList (..))
import HotAir.Maybe (Maybe, just, nothing)
import HotAir.Pair (Pair, pair)

newtype List a
  = List (forall c. c -> (a -> List a -> c) -> c)

nil :: List a
nil = List \n _ -> n

cons :: a -> List a -> List a
cons a as = List \_ c -> c a as

singleton :: a -> List a
singleton a = cons a nil

infixr 7 `cons`

foldr :: (a -> c -> c) -> c -> List a -> c
foldr f c (List l) =
  l c \a as -> f a (foldr f c as)

uncons :: List a -> Maybe (Pair a (List a))
uncons (List l) =
  l nothing \a as -> just (pair a as)

instance IsList (List a) where

  type Item (List a) = a

  toList :: List a -> [a]
  toList = Foldable.toList

  fromList :: [a] -> List a
  fromList = Foldable.foldr cons nil

reverse :: List a -> List a
reverse = Foldable.foldl (flip cons) nil

instance Semigroup (List a) where
  a <> b = foldr cons b a

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f = foldr (\a -> cons (f a)) nil

instance Foldable List where
  foldr = foldr

instance Traversable List where
  traverse
    :: forall f a b. Applicative f
    => (a -> f b)
    -> List a
    -> f (List b)
  traverse a2fb =
    foldr
      (\a fbs -> cons <$> a2fb a <*> fbs)
      (pure nil)
