import Control.Applicative as A
import Data.Traversable    as T

traverse :: Applicative m => (a -> m b) -> [a] -> m [b]
traverse f = foldr step (pure [])
    where step = A.liftA2 (:) . f

-- implement by traverse
transpose :: [[a]] -> [[a]]
transpose []       = []
transpose ([] : _) = []
transpose m        = T.traverse (take 1) m ++ transpose (fmap (drop 1) m)

newtype StateL s a = StateL { runStateL :: s -> (s, a) }

instance Functor (StateL s) where
    fmap f state = StateL $ \s ->
        let (s', a) = runStateL state s
        in  (s', f a)

instance Applicative (StateL s) where
    pure a = StateL $ \s -> (s, a)
    sf <*> sr = StateL $ \s ->
        let (s', f) = runStateL sf s
            (s'', a) = runStateL sr s'
        in  (s'', f a)

mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumL f z bs = runStateL (T.traverse (StateL . flip f) bs) z
