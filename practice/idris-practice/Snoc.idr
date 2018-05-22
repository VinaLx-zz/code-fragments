data SnocList : List a -> Type where
  Empty : SnocList []
  Snoc : SnocList xs -> SnocList (xs ++ [x])

snocList : (xs : List a) -> SnocList xs
snocList xs = snocList' Empty xs
  where snocList' : SnocList front -> (rest: List a) ->
                    SnocList (front ++ rest)
        snocList' {front} snoc [] =
          rewrite appendNilRightNeutral front in
                  snoc
        snocList' {front} snoc (x :: xs) =
          rewrite appendAssociative front [x] xs in
                  (snocList' (Snoc snoc {x}) xs)

myReverse : List a -> List a
myReverse xs with (snocList xs)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | Snoc snoc = x :: myReverse xs | snoc
