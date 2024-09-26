curta :: [a] -> Bool
curta xs = length xs < 3

curta1 :: [a] -> Bool
curta1 xs 
    | tamanho < 3 = True
    | otherwise = False
    where tamanho = length xs
