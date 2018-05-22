module Nat where

data Nat = Zero | Succ Nat deriving (Show)

one :: Nat
one = Succ Zero
two :: Nat
two = Succ one
three :: Nat
three = Succ two

primitive :: Nat -- ^ Proof of Nth natural number
          -> a   -- ^ induction base case
          -> (Nat -> a -> a) -- ^ induction phase, infer (N + 1)th term with Nth term
          -> a -- ^ Result
primitive Zero a _     = a
primitive (Succ n) a f = f n $ primitive n a f

add1 :: Nat -> Nat
add1 n = primitive n one induct
    where induct _ = Succ

-- add 0 m = m
-- add (Succ n) m = Succ (add n m)
add :: Nat -> Nat -> Nat
add n m = primitive n m induct
    where induct _ = Succ

-- mult 0 m = 0
-- mult (Succ n) m = add m (mult n m)
mult :: Nat -> Nat -> Nat
mult n m = primitive n Zero induct
    where induct _ = add m


type EqInduct = Nat -> Bool
eqInduct :: Nat -> EqInduct -> EqInduct
eqInduct _ eqN = \v -> primitive v False induct
    where induct v _ = eqN v
eqZero :: EqInduct
eqZero n = primitive n True $ \_ _ -> False

equal :: Nat -> Nat -> Bool
equal n = primitive n eqZero eqInduct

type GeInduct = EqInduct
geInduct :: Nat -> GeInduct -> GeInduct
geInduct n geN v = not $ not (geN v) || equal v n

geZero :: EqInduct
geZero = const True

ge :: Nat -> Nat -> Bool
ge n m = primitive m geZero geInduct n

search :: (Nat -> Bool) -> Nat -> Nat
search p n = primitive n Zero induct
    where induct :: Nat -> Nat -> Nat
          induct c prev = if p prev then prev else c
