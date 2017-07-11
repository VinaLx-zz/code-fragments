class Functor f => Monoidal f where
    unit :: f ()
    (*&*) :: f a -> f b -> f (a, b)

newtype ZipList a = ZipList { get :: [a] }

instance Functor ZipList where
    fmap f = ZipList . fmap f . get

instance Applicative ZipList where
    pure = ZipList . repeat
    (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)

instance Monoidal ZipList where
    unit = ZipList $ repeat ()
    (ZipList la) *&* (ZipList lb) = ZipList $ zip la lb

instance Monoidal ((->) r) where
    unit = const ()
    f *&* g = \a -> (f a, g a)
