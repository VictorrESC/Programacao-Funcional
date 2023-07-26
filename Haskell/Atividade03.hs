-- IDENTIFICAÇÃO
matricula = "535718" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Victor Emanuel de Sousa Costa" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 3

-- Remove espaços existentes no início
-- e final de uma strings dada.

strip :: [Char] -> [Char]
strip xs = dropWhile (== ' ') (reverse (dropWhile (== ' ') (reverse xs))) 

-- Separa a primeira palavra do restante
-- de uma string (Palavras são substeings 
-- separadas por espaços). Exemplo,

-- >> popWord "casa  de tijolos"
-- ["casa", "de tijolos"]'
-- >>

popWord :: [Char] -> ([Char], [Char])
popWord xs = 
    let(palavraSeparada, restante) = span (/= ' ') xs
    in (palavraSeparada, strip restante)

-- Processa uma string e retorna 
-- a lista de suas palavras. OBS: 
-- palavras não devem ter espaços 
-- extemos e nem serem vazias. Exemplo,

-- >> splitStr " The   fox jumped  "
-- ["The", "fox", "jumped"]

splitStr :: [Char] -> [ [Char] ]
splitStr xs = filter (not . null) (map strip (words xs))