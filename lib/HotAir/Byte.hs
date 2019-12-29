{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Chars #-}
module HotAir.Byte
  ( Byte,
    byte,
    debug,
    toChar,
    fromChar
    )
where

import Control.Applicative (Applicative ((<*>), pure))
import Data.Char (Char)
import qualified Data.Char as Char
import qualified Data.Eq as Builtin
import qualified Data.Foldable as Foldable
import Data.Function (($), (.), id)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Traversable (traverse)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (alignment, peek, poke, sizeOf))
import GHC.Integer (Integer)
import GHC.Num (Num (..))
import GHC.Real (divMod, fromIntegral)
import HotAir.Bit (Bit, zero)
import qualified HotAir.Bit as Bit
import HotAir.Bool ((&&), ifThenElse, true)
import qualified HotAir.Bool as Bool
import HotAir.Eq (Eq ((==)))
import HotAir.Identity (runIdentity)
import HotAir.List (List, reverse)
import HotAir.Pair (pair)
import HotAir.State (evalState, state)
import HotAir.Vector (Vector, cons, nil)
import qualified HotAir.Vector as Vector
import Overloaded.Chars (FromChar)
import qualified Overloaded.Chars
import System.IO (IO)

newtype Byte
  = Byte {toVector :: Vector 8 Bit}

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
  Byte
    $ _1
    `cons` _2
    `cons` _3
    `cons` _4
    `cons` _5
    `cons` _6
    `cons` _7
    `cons` _8
    `cons` nil

bits :: Applicative f => (Bit -> f Bit) -> Byte -> f Byte
bits f (Byte b) =
  Byte <$> traverse f b

toList :: Byte -> List Bit
toList = Vector.toList . toVector

debug :: Byte -> [Word8]
debug = Foldable.foldr ((:) . Bit.toBuiltinNum) [] . toList

toWord8 :: Byte -> Word8
toWord8 =
  Foldable.foldr
    (\bit n -> Bit.toBuiltinNum bit + 2 * n)
    0
    . reverse
    . toList

fromWord8 :: Word8 -> Byte
fromWord8 =
  Byte
    . Vector.reverse
    . Vector.unfoldr
        ( \n ->
            let (n', b) = divMod n 2
             in pair (Bit.fromBuiltinNum b) n'
          )

instance Storable Byte where

  sizeOf :: Byte -> Int
  sizeOf _ = 8

  alignment :: Byte -> Int
  alignment _ = 1

  peek :: Ptr Byte -> IO Byte
  peek = (fromWord8 <$>) . peek @Word8 . castPtr

  poke :: Ptr Byte -> Byte -> IO ()
  poke ptr = poke @Word8 (castPtr ptr) . toWord8

onesCompliment :: Byte -> Byte
onesCompliment =
  runIdentity . bits (pure . Bit.inv)

shift :: Byte -> Byte
shift (Byte b) =
  Byte (Vector.drop @1 b `Vector.append` Vector.singleton zero)

null :: Byte
null =
  byte zero zero zero zero zero zero zero zero

instance Num Byte where

  (+) :: Byte -> Byte -> Byte
  Byte a + Byte b =
    Byte
      $ Vector.reverse
      $ evalState
          ( traverse state
              $ Vector.reverse
              $ Vector.zipWith Bit.fullAdder a b
            )
          Bit.zero

  (-) :: Byte -> Byte -> Byte
  a - b = a + onesCompliment b + 1

  (*) :: Byte -> Byte -> Byte
  (*) a =
    Foldable.sum . evalState (traverse alg (reverse (toList a)))
    where
      alg bit =
        state
          $ pair
          <$> ifThenElse (bit == zero) null
          <*> shift

  abs :: Byte -> Byte
  abs = id

  signum :: Byte -> Byte
  signum b = if b == 0 then 0 else 1

  fromInteger :: Integer -> Byte
  fromInteger = fromWord8 . fromIntegral

instance Eq Byte where
  Byte a == Byte b =
    Foldable.foldr (&&) true (Vector.zipWith (==) a b)

instance Builtin.Eq Byte where
  a == b = Bool.toBuiltin (a == b)

toChar :: Byte -> Char
toChar = Char.chr . fromIntegral . toWord8

fromChar :: Char -> Byte
fromChar = fromWord8 . fromIntegral . Char.ord

instance FromChar Byte where
  fromChar = fromChar

test :: Byte
test = 'a'
