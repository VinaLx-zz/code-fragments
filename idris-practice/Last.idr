data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : Last xs value -> Last (x :: xs) value

nilIsNotLast : Last [] x -> Void
nilIsNotLast l impossible

notLastOne : (x = v -> Void) -> Last [x] v -> Void
notLastOne contra LastOne = contra Refl 

notLastIfNotInTail : {xs : List a} ->
                     (Last (x :: xs) v -> Void) -> Last (x' :: x :: xs) v -> Void
notLastIfNotInTail contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (v : a) -> Dec (Last xs v)
isLast [] v = No nilIsNotLast
isLast (x :: []) v =
  case decEq x v of
      Yes Refl => Yes LastOne
      No notEq => No (notLastOne notEq) 

isLast (x' :: x :: xs) v =
  case isLast (x :: xs) v of
       Yes prf => Yes (LastCons prf)
       No notLast => No (notLastIfNotInTail notLast)

