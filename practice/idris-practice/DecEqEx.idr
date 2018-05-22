import Data.Vect

headUnequal : DecEq a =>
              {xs : Vect n a} -> {ys : Vect n a} -> 
              ((x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl 

tailUnequal : DecEq a =>
              {xs : Vect n a} -> {ys : Vect n a} ->
              ((xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl
