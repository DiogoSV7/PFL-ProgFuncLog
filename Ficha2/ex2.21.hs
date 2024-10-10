algorismos :: Int -> [Int]
algorismos n = algorismosRec n
  where
    algorismosRec n
      | n < 10    = [n] 
      | otherwise = algorismosRec (n `div` 10) ++ [n `mod` 10]
