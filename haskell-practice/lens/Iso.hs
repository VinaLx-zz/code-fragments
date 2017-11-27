{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module Iso where

import Data.Functor.Const
import Data.Functor.Identity

class Profunctor (p :: * -> * -> *) where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    dimap f g = lmap f . rmap g

    lmap :: (a -> b) -> p b c -> p a c
    lmap f = dimap f id

    rmap :: (b -> c) -> p a b -> p a c
    rmap = dimap id

instance Profunctor (->) where
    dimap ab cd f a = cd (f (ab a))

newtype Tagged a b = Tagged { unTagged :: b }

retag :: Tagged s b -> Tagged t b
retag (Tagged b) = Tagged b

newtype Forget r a b = Forget { runForget :: a -> r }

instance Profunctor (Forget r) where
    dimap f _ (Forget k) = Forget (k . f)

instance Functor (Tagged a) where
    fmap f (Tagged b) = Tagged (f b)

instance Profunctor Tagged where
    lmap _ = retag
    rmap = fmap

type Iso s t a b =
    forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type Iso' s a = Iso s s a a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)

from :: Iso s t a b -> Iso b a t s
from i = iso bt sa
    where bt = runIdentity . unTagged . i . Tagged . Identity
          sa = getConst . i Const

