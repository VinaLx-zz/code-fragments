splitWith::(a -> Bool) -> [a] -> [[a]]
splitWith f xs = snd (splitImpl f xs)
    where splitImpl f xs =  case xs of
          [] -> (False, [])
          (x : xs) -> if (f x)
                then (False, snd (splitImpl f tail))
                else case (splitImpl f tail) of
                    (True, xs: xss) -> (True, (x : xs) : xss)
                    (False, xss)    -> (True, [x] : xss)
