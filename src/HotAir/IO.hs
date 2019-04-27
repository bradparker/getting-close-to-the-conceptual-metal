{-# LANGUAGE NoImplicitPrelude #-}

module HotAir.IO
  ( Builtin.IO
  , print
  , putStrLn
  ) where

import qualified System.IO as Builtin

import HotAir.Function ((.))
import HotAir.Show (Show(show))
import HotAir.String (String)
import qualified HotAir.String as String

putStrLn :: String -> Builtin.IO ()
putStrLn = Builtin.putStrLn . String.toBuiltin

print :: Show a => a -> Builtin.IO ()
print = putStrLn . show
