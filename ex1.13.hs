safetail :: [a] -> [a]

 -- Definição de safetail a utilizar guardas

safetail xs
    | null xs   = [] -- Se a lista for nula retorna []
    | otherwise = tail xs -- Caso contrário retorna a cauda da lista


-- Definição de safetail a utilizar expressões condicionais

safetail1 xs = if null xs then [] else tail xs


-- Definição de safetail a utilizar casamento de padrões
safetail2 []     = []
safetail2 (_:xs) = xs
