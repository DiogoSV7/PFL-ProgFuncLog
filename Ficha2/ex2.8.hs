dotprod:: [Float] -> [Float] -> Float
dotprod [] [] = 0
dotprod (x:xs) (y:ys) = x * y + dotprod xs ys

-- Utilizando o zip do enunciado
dotprod2 :: [Float] -> [Float] -> Float
dotprod2 xs ys = sum [x * y | (x, y) <- zip xs ys]
