module Contt where

import Control.Monad (Monad, return)
import Data.Functor.Identity (runIdentity, Identity(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

instance Functor (ContT r m) where
    fmap f = ContT . (. (. f)) . runContT

instance Applicative (ContT r m) where
    pure = ContT . flip ($)
    cf <*> ca = ContT $ \fb -> runContT cf (\f -> runContT ca (fb . f))

instance Monad (ContT r m) where
    return = pure
    ma >>= f = ContT $ \fb -> runContT ma $ \a -> runContT (f a) fb

instance MonadTrans (ContT r) where
    lift = ContT . (>>=)

instance MonadIO m => MonadIO (ContT r m) where
    liftIO = lift . liftIO

evalContT :: Monad m => ContT r m r -> m r
evalContT = ($ return) . runContT

mapContT :: (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f = ContT . fmap f . runContT

withContT :: ((b -> m r) -> a -> m r) -> ContT r m a -> ContT r m b
withContT f = ContT . (. f) . runContT

callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC f = ContT $ \fa -> runContT (f $ \a -> ContT $ const $ fa a) fa

resetT :: Monad m => ContT r m r -> ContT r' m r
resetT = ContT . (>>=) . evalContT

shiftT :: Monad m => ((a -> m r) -> ContT r m r) -> ContT r m a
shiftT = ContT . fmap evalContT

-- (a -> r) -> r
type Cont r = ContT r Identity

cont :: ((a -> r) -> r) -> Cont r a
cont f = ContT $ \c -> Identity $ f (runIdentity . c)

runCont :: Cont r a -> (a -> r) -> r
runCont c = runIdentity . runContT c . fmap Identity

evalCont :: Cont r r -> r
evalCont = runIdentity . evalContT

mapCont :: (r -> r) -> Cont r a -> Cont r a
mapCont = mapContT . map2
    where map2 :: (a -> a) -> Identity a -> Identity a
          map2 f = Identity . f . runIdentity

withCont :: ((b -> r) -> a -> r) -> Cont r a -> Cont r b
withCont = withContT . map2
    where map2 :: ((b -> r) -> a -> r) -> (b -> Identity r) -> a -> Identity r
          map2 f = fmap Identity . f . fmap runIdentity
