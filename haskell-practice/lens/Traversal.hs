{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}



module Traversal where

-- import Lens (Lens, Lens')
import Data.Functor.Const
import Data.Functor.Identity
import Data.Monoid

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

type Getting r s a = (a -> Const r a) -> s -> Const r s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

_all :: Eq a => a -> Traversal' [a] a
_all a f = traverse update
    where update a' = if a' == a then f a else pure a'

over :: Setting s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

set :: Setting s t a b -> b -> s -> t
set l b = over l (const b)

view :: Getting a s a -> s -> a
view l = getConst . l Const

toListOf :: Getting [a] s a -> s -> [a]
toListOf l = getConst . l (Const . return)

preview :: Getting (First a) s a -> s -> Maybe a
preview l = getFirst . getConst . l (Const . First . Just)

has :: Getting Any s a -> s -> Bool
has l = getAny . getConst . l (const (Const (Any True)))

class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
    each :: Traversal s t a b

instance Traversable t => Each (t a) (t b) a b where
    -- each :: (a -> f b) -> t a -> f t b
    each = traverse
