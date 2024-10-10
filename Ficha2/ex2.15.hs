import Data.Char

cifrar::Int -> String -> String
cifrar k xs = [chr (ord x + k) | x <- xs]
