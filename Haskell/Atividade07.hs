-- IDENTIFICAÇÃO

atividade = 7

nome = "Victor Emanuel de Sousa Costa"

matricula = "61815"

-- TIPO DE DADOS

-- Representa polinômio como 
-- um vetor de seus coeficientes

-- através de seus coeficientes

data Poly = Poly [Float]

-- IMPLEMENTAR

-- Instância de Show que permite 
-- imprimir um polinômio


instance Show Poly where
    show (Poly []) = "0.0"
    show (Poly [c]) = show c
    show (Poly (c:cs)) = showTerm c 0 ++ showTerms cs 1
        where
        showTerm :: Float -> Int -> String
        showTerm 0 _ = ""
        showTerm coef 0 = show coef
        showTerm coef 1 = showCoeff coef ++ "x"
        showTerm coef exp = showCoeff coef ++ "x^" ++ show exp

        showCoeff :: Float -> String
        showCoeff 1 = ""
        showCoeff (-1) = "-"
        showCoeff coef = show coef

        showTerms :: [Float] -> Int -> String
        showTerms [] _ = ""
        showTerms (c:cs) exp = case showTerm c exp of
            "" -> showTerms cs (exp + 1)
            term -> " + " ++ term ++ showTerms cs (exp + 1)

-- Exemplos
-- Main> Poly [1,2,3]
-- 1.0+2.0x+3.0X^2
-- *Main> Poly [-2,1,0]
-- -2.0+1.0x
-- *Main> Poly [-1,0,-1]
-- -1.0-1.0X^2


--AVALIAÇÃO DE POLINÔMIOS

-- Avalia um poliômio P 
-- dado x, ou seja, calcula P(x) 

avalPoly :: Poly -> Float -> Float
avalPoly (Poly cs) x = sum $ zipWith (\coef exp -> coef * x^exp) cs [0..]

-- Exemplos
-- *Main> avalPoly (Poly [1,2,3]) 5
-- 86.0
-- *Main> avalPoly (Poly [-1,1,3]) 5
-- 79.0
-- *Main> avalPoly (Poly [11,0,2,2]) 3
-- 83.0