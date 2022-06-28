--Teste 02
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--Questão 01
-- Contagem de repetições 
-- a Ultima letra não esta contando 
contElement :: Char -> String -> Int
contElement letra (a:x)
            |null x = 0
            |letra == a = 1 + contElement letra x
            |otherwise = contElement letra x

analise :: String -> [(Char, Int)]
analise lista = [(e, contElement e lista)| e <- lista]

--Questão 02
--Ordenar elementos por frequencia
--NÃO DEU CERTO

-- ordena :: String -> Char
-- ordena (a:b:x)
--         |null x = a
--         |contElement a x > contElement b x = a
--         |otherwise = b

-- ordenaFreq :: String -> String
-- ordenaFreq (a:x)
--             | null x = []
--             |otherwise = ordena (a:x) : ordenaFreq x