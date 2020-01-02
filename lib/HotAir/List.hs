{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.List
  ( List,
    cons,
    nil,
    foldr,
    unfoldr,
    uncons,
    singleton,
    reverse,
    zipWith,
    take,
    drop,
    iterate,
    concatMap,
    concat
    )
where

import Control.Applicative (Applicative ((<*>), pure))
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Function (($), (.), const, flip, id)
import Data.Functor ((<$>), Functor (fmap))
import Data.Monoid (Monoid (mempty))
import Data.Semigroup (Semigroup ((<>)))
import Data.Traversable (Traversable (traverse))
import GHC.Exts (IsList (..))
import HotAir.Bool (ifThenElse)
import HotAir.Eq ((==))
import HotAir.Maybe (Maybe, fromMaybe, just, maybe, nothing)
import HotAir.Nat (Nat, foldNat, pred, zero)
import HotAir.Pair (Pair, fst, pair, snd)

newtype List a
  = List (forall c. c -> (a -> List a -> c) -> c)

nil :: List a
nil = List $ \n _ -> n

cons :: a -> List a -> List a
cons a as = List $ \_ c -> c a as

infixr 7 `cons`

singleton :: a -> List a
singleton a = cons a nil

foldr :: (a -> c -> c) -> c -> List a -> c
foldr f c (List l) =
  l c $ \a as -> f a (foldr f c as)

unfoldr :: (a -> Maybe (Pair b a)) -> a -> List b
unfoldr f a =
  maybe nil (\ba -> cons (fst ba) (unfoldr f (snd ba))) (f a)

uncons :: List a -> Maybe (Pair a (List a))
uncons (List l) =
  l nothing $ \a as -> just (pair a as)

tail :: List a -> Maybe (List a)
tail = (snd <$>) . uncons

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

instance Monoid (List a) where
  mempty = nil

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

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith z =
  foldr alg (const nil)
  where
    alg a f bs =
      maybe
        nil
        (\ht -> z a (fst ht) `cons` f (snd ht))
        (uncons bs)

take :: Nat -> List a -> List a
take num as =
  foldr alg (const nil) as num
  where
    alg a f n =
      if n == zero
        then nil
        else a `cons` f (pred n)

drop :: Nat -> List a -> List a
drop num as =
  foldNat
    as
    (fromMaybe nil . tail)
    num

iterate :: (a -> a) -> a -> List a
iterate f a = a `cons` iterate f (f a)

concatMap :: (a -> List b) -> List a -> List b
concatMap f = foldr (\a bs -> f a <> bs) nil

concat :: List (List a) -> List a
concat = concatMap id
