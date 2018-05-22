data TakeN: List a -> Type where
  Fewer : TakeN xs
  Exact : (nxs : List a) -> TakeN (nxs ++ rest)

takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z xs = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) with (takeN k xs)
  takeN (S k) (x :: xs) | Fewer = Fewer
  takeN (S k) (x :: (f ++ r)) | Exact _ = Exact (x :: f)

groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (f ++ r) | Exact _ = f :: groupByN n r
