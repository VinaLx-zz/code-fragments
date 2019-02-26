module PropositionLogic

%access public export

data Sum a b = Left a | Right b

And : Type -> Type -> Type
And a b = (a, b)

Or : Type -> Type -> Type
Or a b = Sum a b

Imply : Type -> Type -> Type
Imply a b = a -> b

andIntro : a -> b -> And a b
andIntro a b = (a, b)

andElim1 : And a b -> a
andElim1 = fst -- fst : (a, b) -> a

andElim2 : And a b -> b
andElim2 = snd -- snd : (a, b) -> b

orIntro1 : a -> Or a b
orIntro1 = Left -- Left : a -> Sum a b

orIntro2 : b -> Or a b
orIntro2 = Right -- Right : b -> Sum a b

orElim : Or a b -> Imply a c -> Imply b c -> c
orElim ab ac bc = case ab of
  Left  a => ac a
  Right b => bc b

implElim : Imply a b -> a -> b
implElim ab a = ab a
