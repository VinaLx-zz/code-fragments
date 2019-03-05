-- Implementation of fix-point combinator with recursive type system.
-- fix = λf: T->T. (λx: (μA.A->T). f (x x)) (λx: (μA.A->T). f (x x))
-- fix = λf. (λx. f (x x)) (λx. f (x x))

-- FH is FixHelper
newtype FH a = FH (FH a -> a)

fix :: (a -> a) -> a
fix f = xf (FH xf)
    -- xf :: FH a -> a
    where xf (FH x) = f (x (FH x))

factorial :: Int -> Int
factorial = fix $ \r n ->
    if n == 0 then 1 else n * r (n - 1)
