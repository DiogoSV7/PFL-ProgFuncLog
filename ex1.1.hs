testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = (a < b + c) && (b < a + c) && (c < a + b)

num1 :: Float
num1 = 2.2

num2 :: Float
num2 = 3.3

num3 :: Float
num3 = 4.5