{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.State
  ( StateT,
    evalStateT,
    State,
    state,
    get,
    put,
    modify,
    runState,
    evalState,
    lift
    )
where

import Control.Applicative (Applicative ((<*>), pure))
import Control.Monad (Monad ((>>=), return))
import Data.Function (($), (.))
import Data.Functor (Functor (fmap), (<$>))
import HotAir.Identity (Identity, runIdentity)
import HotAir.Pair (Pair, fst, pair, snd)

newtype StateT s m a
  = StateT {runStateT :: s -> m (Pair a s)}

lift :: Functor m => m a -> StateT s m a
lift act = StateT $ \s -> (`pair` s) <$> act

evalStateT :: Functor m => StateT s m a -> s -> m a
evalStateT s = (fst <$>) . runStateT s

type State s = StateT s Identity

state :: (s -> Pair a s) -> State s a
state s = StateT $ pure . s

runState :: State s a -> s -> Pair a s
runState s = runIdentity . runStateT s

evalState :: State s a -> s -> a
evalState s = fst . runState s

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT s2mas) =
    StateT $ \s -> fmap (\as -> pair (f (fst as)) (snd as)) (s2mas s)

instance Monad m => Applicative (StateT s m) where

  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (pair a s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT s2ma2bs <*> StateT s2mas =
    StateT $ \s -> do
      a2bs <- s2ma2bs s
      as <- s2mas (snd a2bs)
      pure $ pair (fst a2bs (fst as)) (snd as)

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT s2mas >>= f =
    StateT $ \s -> do
      as <- s2mas s
      runStateT (f (fst as)) (snd as)

get :: Applicative m => StateT s m s
get =
  StateT $ \s -> pure (pair s s)

put :: Applicative m => s -> StateT s m ()
put s =
  StateT $ \_ -> pure (pair () s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = do
  s <- get
  put (f s)
