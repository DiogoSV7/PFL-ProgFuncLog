divprop :: Integer -> [Integer]
divprop 1 = []  -- 1 não tem divisores próprios
divprop n = [x | x <- [1..(n-1)], n `mod` x == 0]


primo :: Integer -> Bool 
primo 1 = True
primo x = divprop x == [1]

mersennes::[Int]
mersennes = [(2^n-1) | n <- [1..30], primo(2^n-1)]