 -- a)
num1 = 2

num2 = 9

binom :: Integer -> Integer -> Integer
binom n k = product [1..n] `div` (product [1..k] * product [1..(n-k)])

