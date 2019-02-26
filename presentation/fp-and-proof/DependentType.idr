import PropositionalLogic

andCommute : (A : Type) -> (B : Type) -> And A B -> And B A
andCommute _ _ (a, b) = (b, a)

natEqRefl : (n : Nat) -> n = n
natEqRefl n = Refl

data Even : Nat -> Type

data Odd : Nat -> Type

natEvenOrOdd : (n : Nat) -> Or (Even n) (Odd n)
natEvenOrOdd = ?impl -- ...

symm : a = b -> b = a
symm ab = rewrite ab in Refl

trans : a = b -> b = c -> a = c
trans ab bc = rewrite ab in rewrite bc in Refl
