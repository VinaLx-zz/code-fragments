{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module MyMT.State where

import Control.Monad.State   hiding (State, StateT, runStateT)
import Data.Bifunctor
import Data.Functor.Identity

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
type State s = StateT s Identity

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> fmap (, s) m

instance Functor m => Functor (StateT s m) where
    fmap f (StateT m) = StateT $ \s -> fmap (first f) (m s)

instance Monad m => Applicative (StateT s m) where
    pure = lift . return
    (StateT smf) <*> (StateT sma) = StateT $ \s -> do
        (f, s') <- smf s
        (a, s'') <- sma s'
        return (f a, s'')

instance Monad m => Monad (StateT s m) where
    return = pure
    (StateT m) >>= f = StateT $ \s -> do
        (a, s') <- m s
        let StateT m' = f a
        m' s'

instance Monad m => MonadState s (StateT s m) where
    get = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)
