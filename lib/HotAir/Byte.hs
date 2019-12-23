{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Byte
  ( Byte,
    byte
    )
where

import qualified Data.Foldable as Foldable
import Data.Function ((.), id)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (alignment, peek, poke, sizeOf))
import GHC.Exts (IsList (fromListN))
import GHC.Integer (Integer)
import GHC.Num (Num (..))
import GHC.Real (divMod, fromIntegral)
import HotAir.Bit (Bit (Bit, toBool), zero)
import qualified HotAir.Bit as Bit
import HotAir.Bool (Bool, false, ifThenElse, not, (||))
import HotAir.Eq ((==))
import HotAir.List (List, reverse)
import HotAir.Pair (fst, snd)
import System.IO (IO)

newtype Byte
  = Byte
      ( forall c. ( Bit
                    -> Bit
                    -> Bit
                    -> Bit
                    -> Bit
                    -> Bit
                    -> Bit
                    -> Bit
                    -> c
                    )
        -> c
        )

byte
  :: Bit
  -> Bit
  -> Bit
  -> Bit
  -> Bit
  -> Bit
  -> Bit
  -> Bit
  -> Byte
byte _1 _2 _3 _4 _5 _6 _7 _8 =
  Byte (\b -> b _1 _2 _3 _4 _5 _6 _7 _8)

toList :: Byte -> List Bit
toList (Byte b) =
  b
    ( \_1 _2 _3 _4 _5 _6 _7 _8 ->
        [_1, _2, _3, _4, _5, _6, _7, _8]
      )

toWord8 :: Byte -> Word8
toWord8 =
  Foldable.foldr
    ( \bit n ->
        if Bit.toBool bit
          then 1 + n + n
          else n + n
      )
    0
    . reverse
    . toList

fromWord8 :: Word8 -> Byte
fromWord8 n =
  let (r1, _8) = divMod n 2
      (r2, _7) = divMod r1 2
      (r3, _6) = divMod r2 2
      (r4, _5) = divMod r3 2
      (r5, _4) = divMod r4 2
      (r6, _3) = divMod r5 2
      (r7, _2) = divMod r6 2
      (_, _1) = divMod r7 2
   in byte
        (Bit.fromBuiltinNum _1)
        (Bit.fromBuiltinNum _2)
        (Bit.fromBuiltinNum _3)
        (Bit.fromBuiltinNum _4)
        (Bit.fromBuiltinNum _5)
        (Bit.fromBuiltinNum _6)
        (Bit.fromBuiltinNum _7)
        (Bit.fromBuiltinNum _8)

isZero :: Byte -> Bool
isZero =
  not . Foldable.foldr ((||) . toBool) false . toList

instance Storable Byte where

  sizeOf :: Byte -> Int
  sizeOf _ = 8

  alignment :: Byte -> Int
  alignment _ = 1

  peek :: Ptr Byte -> IO Byte
  peek = (fromWord8 <$>) . peek @Word8 . castPtr

  poke :: Ptr Byte -> Byte -> IO ()
  poke ptr = poke @Word8 (castPtr ptr) . toWord8

onesComplient :: Byte -> Byte
onesComplient (Byte b) =
  b
    ( \_1 _2 _3 _4 _5 _6 _7 _8 ->
        byte
          (Bit (not (toBool _1)))
          (Bit (not (toBool _2)))
          (Bit (not (toBool _3)))
          (Bit (not (toBool _4)))
          (Bit (not (toBool _5)))
          (Bit (not (toBool _6)))
          (Bit (not (toBool _7)))
          (Bit (not (toBool _8)))
      )

shift :: Byte -> Byte
shift (Byte b) =
  b
    ( \_1 _2 _3 _4 _5 _6 _7 _8 ->
        byte _2 _3 _4 _5 _6 _7 _8 zero
      )

null :: Byte
null = byte zero zero zero zero zero zero zero zero

instance Num Byte where

  (+) :: Byte -> Byte -> Byte
  Byte a + Byte b =
    a
      ( \a1 a2 a3 a4 a5 a6 a7 a8 ->
          b
            ( \b1 b2 b3 b4 b5 b6 b7 b8 ->
                let r8 = Bit.halfAdder a8 b8
                    r7 = Bit.fullAdder a7 b7 (snd r8)
                    r6 = Bit.fullAdder a6 b6 (snd r7)
                    r5 = Bit.fullAdder a5 b5 (snd r6)
                    r4 = Bit.fullAdder a4 b4 (snd r5)
                    r3 = Bit.fullAdder a3 b3 (snd r4)
                    r2 = Bit.fullAdder a2 b2 (snd r3)
                    r1 = Bit.fullAdder a1 b1 (snd r2)
                 in byte
                      (fst r1)
                      (fst r2)
                      (fst r3)
                      (fst r4)
                      (fst r5)
                      (fst r6)
                      (fst r7)
                      (fst r8)
              )
        )

  (-) :: Byte -> Byte -> Byte
  a - b = a + onesComplient b + 1

  (*) :: Byte -> Byte -> Byte
  a * Byte b =
    b
      ( \b1 b2 b3 b4 b5 b6 b7 b8 ->
          Foldable.sum @List
            [ if Bit.zero == b8 then null else a,
              if Bit.zero == b7 then null else shift a,
              if Bit.zero == b6 then null else shift (shift a),
              if Bit.zero == b5 then null else shift (shift (shift a)),
              if Bit.zero == b4 then null else shift (shift (shift (shift a))),
              if Bit.zero == b3 then null else shift (shift (shift (shift (shift a)))),
              if Bit.zero == b2 then null else shift (shift (shift (shift (shift (shift a))))),
              if Bit.zero == b1 then null else shift (shift (shift (shift (shift (shift (shift a))))))
              ]
        )

  abs :: Byte -> Byte
  abs = id

  signum :: Byte -> Byte
  signum b = if isZero b then 0 else 1

  fromInteger :: Integer -> Byte
  fromInteger = fromWord8 . fromIntegral
