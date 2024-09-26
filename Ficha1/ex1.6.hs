
num1 :: Float 
num1 = 1

num2 :: Float
num2 = 9

num3 :: Float
num3 = 6

raizes :: Float -> Float -> Float -> Maybe (Float, Float)
raizes a b c
    | discriminant < 0 = Nothing  
    | otherwise        = Just (x1, x2)
  where
    discriminant = b^2 - 4*a*c
    x1 = (-b + sqrt discriminant) / (2*a)
    x2 = (-b - sqrt discriminant) / (2*a)