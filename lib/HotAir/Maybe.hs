{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module HotAir.Maybe
  ( Maybe
  , nothing
  , just
  , maybe
  ) where

newtype Maybe a =
  Maybe (forall c. c -> (a -> c) -> c)

nothing :: Maybe a
nothing = Maybe (\n _ -> n)

just :: a -> Maybe a
just a = Maybe (\_ j -> j a)

maybe :: c -> (a -> c) -> Maybe a -> c
maybe n j (Maybe m) = m n j
