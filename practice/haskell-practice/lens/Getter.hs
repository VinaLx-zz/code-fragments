{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Getter where

import           Control.Applicative  (liftA2)
import           Control.Monad        (void)
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Writer as W
import           Data.Bifunctor       (second)
import           Data.Functor.Const
import qualified Traversal            as T

class Contravariant f where
    contramap :: (a -> b) -> f b -> f a

instance Contravariant (Const a) where
    contramap _ (Const a) = Const a

coerce :: (Functor f, Contravariant f) => f a -> f b
coerce = contramap (const ()) . void

type Getter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

to :: (s -> a) -> Getter s a
to getter f s = coerce $ f (getter s)

view :: R.MonadReader s m => T.Getting a s a -> m a
view g = T.view g <$> R.ask

views :: R.MonadReader s m => T.Getting r s a -> (a -> r) -> m r
views g f = view (g . to f)

listening :: W.MonadWriter w m => T.Getting u w u -> m a -> m (a, u)
listening g m = second (view g) <$> W.listen m

listenings :: W.MonadWriter w m => T.Getting v w u -> (u -> v) -> m a -> m (a, v)
listenings g f = listening (g . to f)

type Acting m r s a = (a -> Effect m r a) -> s -> Effect m r s -- T.Getting (m r) s a
type Action m s a = forall r. Acting m r s a

act :: Monad m => (s -> m a) -> Action m s a
act sma amr s = Effect $ sma s >>= getEffect . amr

perform :: Monad m => Acting m a s a -> s -> m a
perform a = getEffect . a (Effect . return)

newtype Effect m r a = Effect { getEffect :: m r }

instance Functor (Effect m r) where
    fmap _ (Effect m) = Effect m

instance Contravariant (Effect m r) where
    contramap _ (Effect m) = Effect m

instance (Monad m, Monoid r) => Applicative (Effect m r) where
    pure _ = Effect (return mempty)
    Effect ma <*> Effect mb = Effect $ liftA2 mappend ma mb
