{-# LANGUAGE NoImplicitPrelude #-}

module HotAir.Function
  ( (.)
  ) where

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) a = f (g a)
