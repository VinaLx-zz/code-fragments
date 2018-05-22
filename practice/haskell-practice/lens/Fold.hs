{-# LANGUAGE RankNTypes #-}

module Fold where

import           Control.Applicative (liftA2)
import           Getter              (Contravariant, coerce)
import qualified Getter              as G

type Fold s a =
    forall f. (Applicative f, Contravariant f) =>
    (a -> f a) -> s -> f s

newtype Folding f a = Folding { getFolding :: f a }

instance (Applicative f, Contravariant f) => Monoid (Folding f a) where
    mempty = Folding $ coerce $ pure ()
    mappend (Folding fl) (Folding fr) = Folding (fl *> fr)

folded :: Foldable t => Fold (t a) a
folded f = coerce . getFolding . foldMap (Folding . f)

bothF :: Fold (a, a) a
bothF f ~(a1, a2) = liftA2 (,) (f a1) (f a2)

replicated :: Int -> Fold a a
replicated i f a = coerce $ sequenceA (replicate i (f a))
