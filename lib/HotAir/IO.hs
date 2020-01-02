{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.IO
  ( IO,
    print,
    putStrLn
    )
where

import Data.Foldable (sequenceA_)
import Data.Function (($), (.))
import Foreign (allocaBytes, plusPtr, poke)
import HotAir.List (iterate, zipWith)
import qualified HotAir.Nat as Nat
import HotAir.Show (Show (show))
import HotAir.String (String)
import qualified HotAir.String as String
import System.IO (IO, hPutBuf, stdout)

putStr :: String -> IO ()
putStr s =
  let size = Nat.toNum $ String.length s
   in allocaBytes size $ \ptr -> do
        let ptrs = iterate (`plusPtr` 1) ptr
            actions = zipWith poke ptrs (String.toList s)
        sequenceA_ actions
        hPutBuf stdout ptr size

putStrLn :: String -> IO ()
putStrLn s = do
  putStr s
  putStr "\n"

print :: Show a => a -> IO ()
print = putStrLn . show
