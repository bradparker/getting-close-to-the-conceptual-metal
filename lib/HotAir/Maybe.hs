{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Maybe
  ( Maybe,
    nothing,
    just,
    maybe,
    fromMaybe
    )
where

import Control.Applicative (Alternative ((<|>), empty), Applicative ((<*>), pure))
import Control.Monad (Monad ((>>=)))
import Data.Function (($), (.), id)
import Data.Functor ((<$>), Functor (fmap))

newtype Maybe a
  = Maybe (forall c. c -> (a -> c) -> c)

nothing :: Maybe a
nothing = Maybe $ \n _ -> n

just :: a -> Maybe a
just a = Maybe $ \_ j -> j a

maybe :: c -> (a -> c) -> Maybe a -> c
maybe n j (Maybe m) = m n j

fromMaybe :: c -> Maybe c -> c
fromMaybe n = maybe n id

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f = maybe nothing (just . f)

instance Applicative Maybe where

  pure :: a -> Maybe a
  pure = just

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  mf <*> ma = maybe nothing (<$> ma) mf

instance Alternative Maybe where

  empty :: Maybe a
  empty = nothing

  (<|>) :: Maybe a -> Maybe a -> Maybe a
  a <|> b = maybe b just a

instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) ma f = maybe nothing f ma
