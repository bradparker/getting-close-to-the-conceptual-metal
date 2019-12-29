{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.ByteString
  ( putStrLn
    )
where

import Control.Monad ((>>))
import qualified Data.Foldable as Foldable
import Data.Foldable (for_)
import Data.Function (($), (.), flip)
import Data.Functor (fmap)
import Data.Int (Int)
import Data.Semigroup (Semigroup)
import Data.String (IsString (fromString))
import Foreign (allocaBytes, plusPtr)
import Foreign.Storable (poke)
import qualified GHC.Exts as Exts
import HotAir.Byte (Byte)
import qualified HotAir.Byte as Byte
import HotAir.List (List)
import HotAir.State (evalStateT, get, lift, modify)
import System.IO (IO, hPutBuf, stdout)
import Text.Show (Show (show))

newtype ByteString
  = ByteString (List Byte)
  deriving (Semigroup)

toList :: ByteString -> List Byte
toList (ByteString s) = s

fromList :: List Byte -> ByteString
fromList = ByteString

length :: ByteString -> Int
length = Foldable.length . toList

instance IsString ByteString where
  fromString = fromList . fmap Byte.fromChar . Exts.fromList

instance Show ByteString where
  show = Exts.toList . fmap Byte.toChar . toList

putStr :: ByteString -> IO ()
putStr bs =
  allocaBytes (length bs) $ \ptr -> do
    flip evalStateT ptr $ for_ (toList bs) $ \c -> do
      ptr' <- get
      lift $ poke ptr' c
      modify (`plusPtr` 1)
    hPutBuf stdout ptr (length bs)

putStrLn :: ByteString -> IO ()
putStrLn bs = putStr bs >> putStr "\n"
