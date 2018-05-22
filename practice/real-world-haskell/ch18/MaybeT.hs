{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module MaybeT where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State

newtype MaybeT m a = MaybeT {
    runMaybeT :: m (Maybe a)
}

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
bindMT m f = MaybeT $ do
    mb <- runMaybeT m
    case mb of
        Nothing -> return Nothing
        Just j  -> runMaybeT $ f j

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

instance Monad m => Monad (MaybeT m) where
    (>>=) = bindMT
    return = returnMT
    fail = failMT

instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO m = lift $ liftIO m

instance (MonadState s m) => MonadState s (MaybeT m) where
    get = lift get
    put k = lift $ put k
