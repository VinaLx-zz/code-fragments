import PropositionalLogic

orDistrAndL : a -> And (Or a b) (Or a c)
orDistrAndL a = andIntro (orIntro1 a) (orIntro1 a)

orDistrAndR : And b c -> And (Or a b) (Or a c)
orDistrAndR bc = andIntro
  (orIntro2 (andElim1 bc)) (orIntro2 (andElim2 bc))

orDistrAnd : Imply (Or a (And b c)) (And (Or a b) (Or a c))
orDistrAnd assumption =
  orElim assumption orDistrAndL orDistrAndR
