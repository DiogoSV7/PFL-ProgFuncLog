max3, min3 :: Ord a=> a->a->a->a

max3 x y z = max x (max y z)

min3 x y z = min x (min y z)

-- Esta aproach é da 1.11.a) e a 1.11.b)
