import Data.Char

asInt :: String -> Integer
asInt ('+' : s) = asInt s
asInt ('-' : s) = - (asInt s)
asInt s = foldl accumulate (toInteger 0) s
    where accumulate i c | isDigit c = toInteger 10 * i + toInteger (digitToInt c)
                         | otherwise = error "need digit!"
