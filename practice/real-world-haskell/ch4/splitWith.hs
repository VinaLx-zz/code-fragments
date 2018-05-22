splitWith::(a -> Bool) -> [a] -> [[a]]
splitWith f xs = snd (splitImpl f xs)
    where splitImpl f xs = case xs of
        (x : xs) | not (f x) -> case (splitImpl f tail) of
            (True, xs: xss) -> (True, (x : xs) : xss)
            (False, xss)    -> (True, [x] : xss)
        (x : xs)  | f x  -> (False, snd (splitImpl f tail))
        [] -> (False, [])
