divprop :: Integer -> [Integer]
divprop 1 = []  -- 1 não tem divisores próprios
divprop n = [x | x <- [1..(n-1)], n `mod` x == 0]

-- Função que verifica se um número é primo
primo :: Integer -> Bool 
primo 1 = True
primo x = divprop x == [1]