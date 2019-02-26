data ProductNatBool : Type where
  Product : Nat -> Bool -> ProductNatBool

ProductNatBool' : Type
ProductNatBool' = (Nat, Bool)

data SumNatBool : Type where
  SumNat  : Nat  -> SumNatBool
  SumBool : Bool -> SumNatBool

data Shape : Type where
  Triangle  : Double -> Double -> Shape
  Rectangle : Double -> Double -> Shape
  Circle    : Double -> Shape

data Bool : Type where
  True : Bool
  False : Bool
