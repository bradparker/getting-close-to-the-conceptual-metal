{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HotAir.Bit
  ( Bit
  , toBool
  , one
  , zero
  , (.|.)
  , (.&.)
  , xor
  , toBuiltinNum
  , fromBuiltinNum
  , halfAdder
  , fullAdder
  ) where

import qualified Data.Eq as Builtin
import qualified GHC.Num as Builtin
import HotAir.Bool (Bool, true, false, (&&), (||), ifThenElse)
import HotAir.Pair (Pair, pair, fst, snd)
import HotAir.Eq (Eq((==), (/=)))

newtype Bit =
  Bit { toBool :: Bool }
  deriving Eq

one :: Bit
one = Bit true

zero :: Bit
zero = Bit false

(.|.) :: Bit -> Bit -> Bit
Bit a .|. Bit b = Bit (a || b)

(.&.) :: Bit -> Bit -> Bit
Bit a .&. Bit b = Bit (a && b)

xor :: Bit -> Bit -> Bit
xor (Bit a) (Bit b) = Bit (a /= b)

halfAdder :: Bit -> Bit -> Pair Bit Bit
halfAdder a b = pair (a `xor` b) (a .&. b)

fullAdder :: Bit -> Bit -> Bit -> Pair Bit Bit
fullAdder a b c =
  let sc  = halfAdder a b
      sc' = halfAdder (fst sc) c
  in  pair (fst sc') (snd sc' .|. snd sc)

toBuiltinNum :: Builtin.Num a => Bit -> a
toBuiltinNum (Bit b) = ifThenElse b 1 0

fromBuiltinNum :: (Builtin.Num a, Builtin.Eq a) => a -> Bit
fromBuiltinNum 0 = zero
fromBuiltinNum _ = one
