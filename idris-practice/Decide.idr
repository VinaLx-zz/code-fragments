data Decide : (prop : Type) -> Type where
  Yes : (prf : prop) -> Decide prop
  No : (contra : prop -> Void) -> Decide prop

zeroNotSuc : (0 = S k) -> Void
zeroNotSuc Refl impossible

sucNotZero : (S k = 0) -> Void
sucNotZero Refl impossible

noRec : ((k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl

checkEqNat : (n : Nat) -> (m : Nat) -> Decide (n = m)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S k) = No zeroNotSuc
checkEqNat (S k) Z = No sucNotZero
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Yes Refl => Yes Refl
                              No contra => No (noRec contra)
