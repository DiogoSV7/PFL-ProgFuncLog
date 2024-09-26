imc :: Int -> Float -> String

imc peso altura = 
    let imczin = fromIntegral peso /(altura*altura)
    in if imczin < 18.5 then "Abaixo do peso"
    else if imczin < 25 then "Peso normal"
    else if imczin < 30 then "Excesso de Peso"
    else "Obesidade"
    
