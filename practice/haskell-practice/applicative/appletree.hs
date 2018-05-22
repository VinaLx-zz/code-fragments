data AT a = L a | B (AT a) (AT a)

instance Functor AT where
    fmap f (L a)   = L $ f a
    fmap f (B a b) = B (fmap f a) (fmap f b)

instance Applicative AT where
    pure = L
    (<*>) (L f) (L a)        = L $ f a
    (<*>) lf @ (L _) (B a b) = B (lf <*> a) (lf <*> b)
    (<*>) (B fa fb) t        = B (fa <*> t) (fb <*> t)

instance Monad AT where
    return = L
    (>>=) (L a) f   = f a
    (>>=) (B a b) f = B (a >>= f) (b >>= f)

ap :: AT (a -> b) -> AT a -> AT b
ap ft t = ft >>= \f -> t >>= return . f

fructify :: AT a -> AT a
fructify = (=<<) $ \a -> B (pure a) (pure a)

prune :: a -> (a -> Bool) -> AT a -> AT a
prune z p t = case t of
    B (L a) _ | p a -> L z
    B _ (L a) | p a -> L z
    l @ (L _) -> l
    B a b     -> B (prune z p a) (prune z p b)

reproduce :: (a -> b) -> (a -> b) -> AT a -> AT b
reproduce f g = (<*>) $ B (pure f) (pure g)
