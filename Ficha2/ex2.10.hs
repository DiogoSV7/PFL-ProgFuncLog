divprop :: Integer -> [Integer]
divprop 1 = []  -- 1 não tem divisores próprios
divprop n = [x | x <- [1..(n-1)], n `mod` x == 0]


perfeitos:: Integer -> [Integer]
perfeitos n = [x | x <- [1..n], sum (divprop x)==x ]