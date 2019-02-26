import PropositionalLogic

%hide (<=)

data (<=) : Nat -> Nat -> Type where
  LeN : {n : Nat} -> n <= n
  LeS : {m : Nat} -> {n : Nat} -> m <= n -> m <= S n

infix 6 <=

plusRightS : (m : Nat) -> (n : Nat) -> m + S n = S m + n 
plusRightS Z n = Refl
plusRightS (S m) n = rewrite plusRightS m n in Refl

zeroLeN : (n : Nat) -> Z <= n
zeroLeN Z = LeN
zeroLeN (S n) = LeS (zeroLeN n)

nLeS : {m : Nat} -> {n : Nat} -> m <= n -> S m <= S n
nLeS LeN = LeN
nLeS (LeS m_le_pn) = LeS (nLeS m_le_pn)

leRelax : {m : Nat} -> {n : Nat} -> S m <= n -> m <= n
leRelax LeN = LeS LeN
leRelax (LeS sm_le_pn) = LeS (leRelax sm_le_pn)

plusLe : (a : Nat) -> (b : Nat) -> (c : Nat) -> a + b = c -> b <= c
plusLe Z b c b_eq_c = rewrite b_eq_c in LeN
plusLe (S a') b c eq = leRelax -- S b <= c
  -- induction on `a`
  (plusLe a' (S b) c
    -- a' + S b = c
    (replace {P = \p => p = c} (sym (plusRightS a' b)) eq))

plusLeSplit : (a : Nat) -> (b : Nat) -> (c : Nat) -> (d : Nat) ->
              a + b = c + d -> Or (a <= c) (b <= d)
plusLeSplit Z b c d eq = orIntro1 (zeroLeN c)
plusLeSplit a b Z d eq = orIntro2 (plusLe a b d eq)
plusLeSplit (S a') b (S c') d eq =
  case plusLeSplit a' b c' d (succInjective (a' + b) (c' + d) eq) of
       Left a'_le_c' => orIntro1 (nLeS a'_le_c')
       Right b_le_d  => orIntro2 b_le_d


