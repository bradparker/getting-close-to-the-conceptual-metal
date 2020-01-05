{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Eq
  ( Eq ((==), (/=))
    )
where

import HotAir.Bool (Bool, not)

class Eq a where

  (==) :: a -> a -> Bool

  (/=) :: a -> a -> Bool
  a /= b = not (a == b)
