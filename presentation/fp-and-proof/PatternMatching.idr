%hide List

data List a = Nil | Cons a (List a)

length : List a -> Int
length l = case l of
  Nil => 0
  Cons h t => 1 + length t
