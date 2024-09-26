-- Conversão de números de 0 a 19
unidades :: Int -> String
unidades n = case n of
    0  -> "Zero"
    1  -> "Um"
    2  -> "Dois"
    3  -> "Três"
    4  -> "Quatro"
    5  -> "Cinco"
    6  -> "Seis"
    7  -> "Sete"
    8  -> "Oito"
    9  -> "Nove"
    10 -> "Dez"
    11 -> "Onze"
    12 -> "Doze"
    13 -> "Treze"
    14 -> "Catorze"
    15 -> "Quinze"
    16 -> "Dezesseis"
    17 -> "Dezessete"
    18 -> "Dezoito"
    19 -> "Dezenove"

-- Conversão das dezenas de 20 a 90
dezenas :: Int -> String
dezenas n = case n of
    2 -> "Vinte"
    3 -> "Trinta"
    4 -> "Quarenta"
    5 -> "Cinquenta"
    6 -> "Sessenta"
    7 -> "Setenta"
    8 -> "Oitenta"
    9 -> "Noventa"
    _ -> ""

-- Converter números menores que 100
converte :: Int -> String
converte n
    | n < 20 = unidades n
    | otherwise =
        let d = n `div` 10  
            u = n `mod` 10  
        in if u == 0 then dezenas d else dezenas d ++ " e " ++ unidades u
