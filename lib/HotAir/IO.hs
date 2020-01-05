{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.IO
  ( IO,
    print,
    putStrLn
    )
where

import Data.Function ((.))
import Data.Foldable (traverse_)
import HotAir.Char (Char)
import qualified HotAir.Char as Char
import HotAir.Show (Show (show))
import HotAir.String (String)
import qualified HotAir.String as String
import System.IO (IO)
import qualified System.IO as IO

putChar :: Char -> IO ()
putChar = IO.putChar . Char.toBuiltin

putStr :: String -> IO ()
putStr = traverse_ putChar . String.toList

putStrLn :: String -> IO ()
putStrLn s = do
  putStr s
  putStr "\n"

print :: Show a => a -> IO ()
print = putStrLn . show
