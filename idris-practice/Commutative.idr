myPlusCommutes : (n: Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z n = rewrite plusZeroRightNeutral n in Refl
myPlusCommutes (S k) n = rewrite myPlusCommutes k n in
                         rewrite plusSuccRightSucc n k in Refl
