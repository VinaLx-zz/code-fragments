import Data.Vect

valueNeverInNil : Elem value [] -> Void
valueNeverInNil e impossible

valueNotInList :  (Elem v xs -> Void) -> (v = x -> Void) -> Elem v (x :: xs) -> Void
valueNotInList notThere notHere Here = notHere Refl
valueNotInList notThere notHere (There later) = notThere later

myIsElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
myIsElem value [] = No valueNeverInNil
myIsElem value (x :: xs) =
  case decEq value x of
       Yes Refl => Yes Here
       No notHere => case myIsElem value xs of
                         Yes prf => Yes (There prf)
                         No notThere => No (valueNotInList notThere notHere)

