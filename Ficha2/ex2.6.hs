somaquadrados:: Int -> Int
somaquadrados 0 = 0
somaquadrados n = n^2 + somaquadrados (n-1)

