lista = [1,2,3,4,5,6,7,8]

tamanho = length lista -1 

drop1 = drop tamanho lista

take1 = take 1 drop1

 -- Uma aproach é retirar todos e depois ficar apenas com o ultimo

reversed_list = reverse lista

ultimo_elemento = take 1 reversed_list