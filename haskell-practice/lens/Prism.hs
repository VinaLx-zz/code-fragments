{-# LANGUAGE RankNTypes #-}

module Prism where

import Data.Functor.Identity
import Iso                   as I

class I.Profunctor p => Choice p where
    left' :: p a b -> p (Either a c) (Either b c)

    right' :: p a b -> p (Either c a) (Either c b)
    right' = dimap swp swp . left'
        where swp (Left a)  = Right a
              swp (Right a) = Left a

instance Choice I.Tagged where
    left' (Tagged b) = Tagged $ Left b


type Prism s t a b =
    forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt sta = dimap sta (either pure (fmap bt)) . right'

prism' :: (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' as sma = prism as f
    where f s = case sma s of
                    Nothing -> Left s
                    Just a  -> Right a

type AReview t b = Tagged b (Identity b) -> Tagged t (Identity t)

review :: AReview s a -> a -> s
review p = runIdentity . I.unTagged . p . I.Tagged . Identity

data Market a b s t = Market (b -> t) (s -> Either t a)

instance Functor (Market a b s) where
    fmap f (Market bt sta) = Market (f . bt) (either (Left . f) Right . sta)

instance Profunctor (Market a b) where
    lmap xs (Market bt sta) = Market bt (sta . xs)
    rmap = fmap

instance Choice (Market a b) where
    left' (Market bt sta) = Market (Left . bt) f
        where f (Left s)  = either (Left . Left) Right (sta s)
              f (Right c) = Left (Right c)

unPrism :: Prism s t a b -> (b -> t, s -> Either t a)
unPrism p = (bt, sta)
    where (Market bit sita) = p (Market Identity Right)
          bt = runIdentity . bit
          sta = either (Left . runIdentity) Right . sita
