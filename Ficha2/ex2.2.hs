intersperse:: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse a (x:xs) = x : a : intersperse a xs