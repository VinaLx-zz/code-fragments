import Data.List

sortByLen :: [[a]] -> [[a]]
sortByLen = sortBy compLen
    where compLen l1 l2 = compare (length l1) (length l2)
