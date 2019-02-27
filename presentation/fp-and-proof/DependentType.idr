import PropositionalLogic

%hide List
%hide NonEmpty
%hide head

%default total

andCommute : (A : Type) -> (B : Type) -> And A B -> And B A
andCommute _ _ (a, b) = (b, a)

natEqRefl : (n : Nat) -> n = n
natEqRefl n = Refl

data Even : Nat -> Type

data Odd : Nat -> Type

natEvenOrOdd : (n : Nat) -> Or (Even n) (Odd n)
natEvenOrOdd = ?impl -- ...

data List : Type -> Type where
  Nil  : List a
  Cons : a -> List a -> List a

length : List a -> Nat
length Nil = 0
length (Cons h t) = 1 + length t

nonEmpty : List a -> Bool
nonEmpty Nil = False
nonEmpty (Cons h t) = True

data NonEmpty : List a -> Type where
  IsNonEmpty : (h : a) -> (t : List a) -> NonEmpty (Cons h t)

head : (l : List a) -> {prf : NonEmpty l} -> a
head (Cons h t) = h

symm : a = b -> b = a
symm ab = rewrite ab in Refl

trans : a = b -> b = c -> a = c
trans ab bc = rewrite ab in rewrite bc in Refl
