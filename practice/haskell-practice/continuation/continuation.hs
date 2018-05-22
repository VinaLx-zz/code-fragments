module Cont where

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
    fmap f c = Cont $ \bf -> runCont c $ \a -> bf $ f a

instance Applicative (Cont r) where
    pure a = Cont ($ a)
    cf <*> ca = Cont $ \bf ->
        runCont cf $ \f ->
        runCont ca $ \a -> bf $ f a

instance Monad (Cont r) where
    c >>= f = Cont $ \bf -> runCont c $ \a -> runCont (f a) bf

-- | Call with Current Continuation
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \fa -> runCont (f (\a -> Cont $ \_ -> fa a)) fa

evalCont :: Cont r r -> r
evalCont = ($ id) . runCont

mapCont :: (r -> r) -> Cont r a -> Cont r a
mapCont f = Cont . (f .) . runCont

withCont :: ((b -> r) -> a -> r) -> Cont r a -> Cont r b
withCont f ca = Cont $ \br -> runCont ca (f br)

reset :: Cont r r -> Cont r' r
reset = Cont . flip ($) . evalCont

shift :: ((a -> r) -> Cont r r) -> Cont r a
shift f = Cont $ \ar -> evalCont (f ar)

