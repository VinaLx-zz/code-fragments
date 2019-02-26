import PropositionalLogic

%hide not
%hide absurd
%hide Void

data Void : Type where
  -- no constructor

void : {a : Type} -> Void -> a
void v impossible

not : Type -> Type
not P = P -> Void

-- Conj P (P -> Void) -> Void
contradiction : {P : Type} -> not (Conj P (not P))
contradiction (p, notp) = notp p

notEqualCommute : not (a = b) -> not (b = a)
notEqualCommute not_ab ab = not_ab (sym ab)

discriminateNat : {n : Nat} -> not (Z = S n)
discriminateNat Refl impossible

truthIrrefutable : {P : Type} -> P -> not (not P)
-- curry : ((a, b) -> c) -> a -> b -> c
-- contradiction    : (P, not P) -> Void
-- truthIrrefutable : P -> not P -> Void
truthIrrefutable = curry contradiction

notLoemContra : {P : Type} ->
                not (Or P (not P)) -> And (not P) (not (not P))
notLoemContra not_loem = (notp, notnotp)
  where notp    p  = not_loem (orIntro1 p)
        notnotp np = not_loem (orIntro2 np)

loemIrrefutable : {P : Type} -> not (not (Or P (not P)))
loemIrrefutable not_loem = contradiction (notLoemContra not_loem)

