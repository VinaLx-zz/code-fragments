{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}


module Getter where

import qualified Control.Monad.Reader as R
import qualified Control.Monad.Writer as W
import           Data.Bifunctor       (second)
import           Data.Functor.Const
import qualified Traversal            as T

type Getter s a = forall r. T.Getting r s a

to :: (s -> a) -> Getter s a
to getter f s = Const $ getConst $ f $ getter s

view :: R.MonadReader s m => T.Getting a s a -> m a
view g = T.view g <$> R.ask

views :: R.MonadReader s m => T.Getting r s a -> (a -> r) -> m r
views g f = view (g . to f)

listening :: W.MonadWriter w m => T.Getting u w u -> m a -> m (a, u)
listening g m = second (view g) <$> W.listen m

listenings :: W.MonadWriter w m => T.Getting v w u -> (u -> v) -> m a -> m (a, v)
listenings g f = listening (g . to f)
