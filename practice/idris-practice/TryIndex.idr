module TryIndex

import Data.Vect

||| try to get element in vector
tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} x xs = 
  case integerToFin x n of
       Nothing  => Nothing
       (Just f) => Just (index f xs)

vTake : (f: Fin n) -> Vect n a -> Vect (finToNat f) a
vTake FZ _ = Nil
vTake (FS x) (h :: xs) = h :: vTake x xs
