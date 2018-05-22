module Predicate

import Data.Vect

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip = ?zip

tryZip : Vect m a -> Vect n b -> Maybe (Vect m (a, b))

{- -- did m == n say anything about equality? -}
{- wrongTryZip : Vect m a -> Vect n b -> Maybe (Vect m (a, b)) -}
{- wrongTryZip {m} {n} v1 v2 = -}
  {- if m == n -}
    {- then Just (zip v1 v2) -}
    {- else Nothing -}

equal : (m: Nat) -> (n: Nat) -> Maybe (m = n)
equal Z Z = Just Refl
equal Z nonzero = Nothing
equal nonzero Z = Nothing
equal (S m) (S n) =
  case equal m n of
      Nothing => Nothing
      (Just Refl) => Just Refl

tryZip {m} {n} v1 v2 =
  case equal m n of
      Nothing => Nothing
      -- the proof here tell compiler m and n are the same
      (Just Refl) => Just (zip v1 v2)
