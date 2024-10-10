-- Função para concatenar duas listas de caracteres
concatrec :: [Char] -> [Char] -> [Char]
concatrec [] ys = ys
concatrec (x:xs) ys = x : concatrec xs ys

-- Função para replicar cada caractere de uma lista
replicaterec :: [Char] -> [Char]
replicaterec [] = []
replicaterec (x:xs) = concatrec (replicate 2 x) (replicaterec xs)
