%hide List

data List : Type -> Type where
  Nil  : List a
  Cons : a -> List a -> List a

data Sum : Type -> Type -> Type where
  Left  : a -> Sum a b
  Right : b -> Sum a b

data Product : Type -> Type -> Type where
  MkProduct : a -> b -> Product a b

Product' : Type -> Type -> Type
Product' a b = (a, b)
