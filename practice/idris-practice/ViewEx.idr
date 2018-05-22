import Data.List.Views
import Data.Vect
import Data.Vect.Views
import Data.Nat.Views

total
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix [] ys | Empty = []
  equalSuffix (xs ++ [x]) ys | (Snoc xrec) with (snocList ys)
    equalSuffix (xs ++ [x]) [] | (Snoc xrec) | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xrec) | (Snoc yrec) =
      if x == y then equalSuffix xs ys | xrec | yrec ++ [x]
                else []

total
mergeSort : Ord a => Vect n a -> Vect n a
mergeSort xs with (splitRec xs)
  mergeSort [] | _ = []
  mergeSort [x] | _ = [x]
  mergeSort (xs ++ ys) | SplitRecPair lrec rrec =
    merge (mergeSort xs | lrec) (mergeSort ys | rrec)

total
toBinary : Nat -> String
toBinary n with (halfRec n)
  toBinary Z | HalfRecZ = ""
  toBinary (S (n + n)) | HalfRecOdd rec = toBinary n | rec ++ "1"
  toBinary (n + n) | HalfRecEven rec = toBinary n | rec ++ "0"


total
palindrome : String -> Bool
palindrome xs = palindrome' (unpack xs)
  where palindrome' : List Char -> Bool
        palindrome' xs with (vList xs)
          palindrome' [] | _ = True
          palindrome' [x] | _ = True
          palindrome' (x :: xs ++ [y]) | VCons rec =
            if x == y then palindrome' xs | rec else False
