module Continuation where

newtype Continuation r a = Cont { run :: (a -> r) -> r }

instance Functor (Continuation r) where
    fmap f c = Cont $ \bf -> run c $ \a -> bf $ f a

instance Applicative (Continuation r) where
    pure a = Cont ($ a)
    cf <*> ca = Cont $ \bf ->
        run cf $ \f ->
        run ca $ \a -> bf $ f a

instance Monad (Continuation r) where
    c >>= f = Cont $ \bf -> run c $ \a -> run (f a) bf

myCallCC :: ((a -> Continuation r b) -> Continuation r a) -> Continuation r a
myCallCC f = Cont $ \fa -> run (f (\a -> Cont $ \_ -> fa a)) fa
