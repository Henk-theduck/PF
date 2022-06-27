
 -- Lista da monitoria 
 -- Questão 1
 -- Função que retorna se um elemento pertence ou não a uma lista
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- pertence :: Int -> [Int] -> Bool
-- pertence num (a:x)
--             | num == a = True
--             | null x = False
--             | otherwise = pertence num x

-- maior :: [Int] -> Int
-- maior (a:b:x)
--         | null x = a
--         | a > b = maior (a:x)
--         | otherwise = maior (b:x)

auxN :: Int -> Int -> [a] -> a
auxN n cont (a:x)
        | n == cont = a
        | otherwise = auxN n (cont+1) x

retornaN :: Int -> [a] -> a
retornaN n (a:x)
        | n == 0 = a
        |otherwise =  auxN n 1 x

retiraN :: Int -> [Int] -> [Int]
retiraN n lista = [e | e <- lista, e /= lista !! n]
