-- a)
minimmum :: Ord a => [a] -> a
minimmum [] = error "lista vazia"
minimmum [x] = x
minimmum (x:xs) =
    let minTail = minimmum xs  -- Chamada recursiva
    in if x < minTail then x else minTail

-- b)
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:ys) | x == y = ys
                | otherwise = y : delete x ys

-- c)
ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort l = m : ssort (delete m l)
    where m = minimmum l
