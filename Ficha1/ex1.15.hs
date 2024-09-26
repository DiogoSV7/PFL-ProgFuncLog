mediana :: Float -> Float -> Float -> Float

mediana a b c = a + b + c - (max a (max b c)) - (min a (min b c))

