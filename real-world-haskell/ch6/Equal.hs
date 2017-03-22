class Equal a where
    isEqual' :: a -> a -> Bool
    isEqual' aa b = not (isNotEqual' aa b)
    isNotEqual' :: a -> a -> Bool

instance Equal Int where
    isEqual' a b = a == b

-- infinite loop
instance Equal Integer
