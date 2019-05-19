{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module HotAir.Either
  ( Either
  , left
  , right
  , either
  ) where

newtype Either a b =
  Either (forall c. (a -> c) -> (b -> c) -> c)

left :: a -> Either a b
left a = Either (\l _ -> l a)

right :: b -> Either a b
right b = Either (\_ r -> r b)

either :: (a -> c) -> (b -> c) -> Either a b -> c
either l r (Either e) = e l r
