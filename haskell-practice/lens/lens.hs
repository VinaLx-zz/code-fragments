{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Lens where

import Data.Bifunctor
import Data.Functor.Compose
import Data.Functor.Identity

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (, x) <$> f a

_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (x, ) <$> f a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set f s = set s <$> f (get s)

choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 _ f (Left s1)  = Left <$> l1 f s1
choosing _ l2 f (Right s2) = Right <$> l2 f s2

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = second runIdentity $ getCompose $
    l (\a -> let b = f a in Compose (b, Identity b)) s

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = second runIdentity $ getCompose $
    l (\a -> Compose (a, Identity $ f a)) s

united :: Lens' s ()
united f s = const s <$> f ()
