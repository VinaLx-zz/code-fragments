module Match where

data List a = Null | Cons a (List a)

tail :: List a -> List a
tail Null = Null
tail (Cons a t) = t

tail2 :: List a -> List a
tail2 Null = Null
tail2 (Cons a Null) = Null
tail2 (Cons a (Cons a' t)) = t
