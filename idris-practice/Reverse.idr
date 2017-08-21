import Data.Vect

prf : Vect (S n + m) a -> Vect (plus n (S m)) a
prf {n} {m} p = rewrite sym (plusSuccRightSucc n m) in p

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
        reverse' {n} acc [] = rewrite plusZeroRightNeutral n in acc
        reverse' acc (x :: xs) = prf (reverse' (x :: acc) xs)
