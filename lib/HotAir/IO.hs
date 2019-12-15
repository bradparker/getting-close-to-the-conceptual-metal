{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.IO
  ( Builtin.IO,
    print,
    putStrLn
    )
where

import Data.Function ((.))
import HotAir.Show (Show (show))
import HotAir.String (String)
import qualified HotAir.String as String
import qualified System.IO as Builtin

putStrLn :: String -> Builtin.IO ()
putStrLn = Builtin.putStrLn . String.toBuiltin

print :: Show a => a -> Builtin.IO ()
print = putStrLn . show
