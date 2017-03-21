intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ [x] = x
intersperse i (x: xs) = x ++ [i] ++ intersperse i xs
