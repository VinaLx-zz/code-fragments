%hide (+)

%access export

(+) : Nat -> Nat -> Nat
Z + n = n
(S m) + n = S (m + n)

plusLeftIdentity : (n : Nat) -> Z + n = n
plusLeftIdentity n = Refl

plusRightIdentity : (n : Nat) -> n + Z = n
plusRightIdentity Z = Refl
plusRightIdentity (S n) = rewrite plusRightIdentity n in Refl

plusRightS : (m : Nat) -> (n : Nat) -> m + S n = S m + n 
plusRightS Z n = Refl
plusRightS (S m) n = rewrite plusRightS m n in Refl

plusCommute : (m : Nat) -> (n : Nat) -> m + n = n + m
plusCommute Z n = sym (plusRightIdentity n)
plusCommute (S m) n =
  -- S (m + n) = n + S m
  rewrite plusCommute m n in
  -- S (n + m) = n + S m
  rewrite plusRightS n m in
  -- S (n + m) = S n + m
  Refl
