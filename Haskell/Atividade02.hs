-- IDENTIFICAÇÃO
matricula = "535718" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Victor Emanuel de Sousa Costa" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 2

-- Esta atividade visa construir uma 
-- função que determine os n primeitos números primos

-- Construa as funções a seguir:

-- determina os divisores de x excluindo o 1
divisores :: Int -> [Int]
divisores x = [d | d <- [2..x], mod x d == 0] 
{--
A função divisores recebe um número inteiro x como entrada e retorna uma lista com os divisores de x, excluindo o número 1. Para fazer isso, utilizamos uma list comprehension que varre todos os números entre 2 e x-1 e seleciona apenas aqueles que são divisores de x. A expressão mod x d == 0 testa se o número d é divisor de x.
--}

-- Determina se um números x é ou não primo
eprimo :: Int -> Bool
eprimo x = null (divisores x)
{--A função eprimo recebe um número inteiro x como entrada e retorna True se x é primo, e False caso contrário. Para fazer isso, utilizamos a função divisores que acabamos de definir. Se a lista de divisores de x (excluindo o número 1) é vazia, então x é primo, caso contrário, não é primo.--} 

-- cria lista com n primeiros primos
primos :: Int -> [Int]
primos n = take n [x | x <- [2..], eprimo x] 
{--A função primos recebe um número inteiro n como entrada e retorna uma lista com os n primeiros números primos. Para fazer isso, utilizamos a função eprimo que definimos anteriormente. Começamos criando uma lista infinita com todos os números a partir do 2, utilizando [2..]. Em seguida, filtramos apenas os números que são primos utilizando a função eprimo. Por fim, utilizamos a função take para retornar apenas os primeiros n elementos da lista de números primos.--}