insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]  -- Se a lista está vazia, retorna a lista com o elemento x
insert x (y:ys)
  | x <= y    = x : y : ys  -- Se x é menor ou igual a y, insere x antes de y
  | otherwise = y : insert x ys

isort:: Ord a => [a] -> [a]
isort = foldr insert []