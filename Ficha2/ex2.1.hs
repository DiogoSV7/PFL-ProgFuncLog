-- a)
myand:: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand xs


-- b)
myor:: [Bool] -> Bool
myor [] = False 
myor (x:xs) = x || myor xs

-- c)
myconcat:: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

-- d)
myreplicate:: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = x : myreplicate (n-1) x

-- e)
myselect:: [a] -> Int -> a
myselect (x:xs) 0 = x
myselect (x:xs) n = myselect xs (n-1)

-- f)
myelem:: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem x (y:ys) = x == y || myelem x ys