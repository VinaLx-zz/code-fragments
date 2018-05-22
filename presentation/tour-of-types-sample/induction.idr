module Induction

data Natural = Zero
             | Succ Natural

data List a = Null
            | Cons a (List a)

-- illustration purpose, need extra refinement
-- Nil : Vect 0 a
concat : Vect m a -> Vect n a -> Vect (m + n) a
-- base case: Vect 0 a -> Vect n a -> Vect (0 + n) a
concat Nil v = v
-- induction case:
-- given Vect k a -> Vect n a -> Vect (k + n) a
-- prove Vect (1 + k) a -> Vect n a -> Vect (1 + k + n) a
concat (a :: as) v = a :: concat as v
