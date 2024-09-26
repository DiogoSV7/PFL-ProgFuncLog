classifica :: Int->String
classifica x
    | x <= 9 = "Reprovado"
    | x >= 10 && x<=12 = "Suficiente"
    | x >= 13 && x<=15 = "Bom"
    | x >= 16 && x<=18 = "Muito Bom"
    | x >= 19 && x<=20 = "Muito Bom com Distincao"

