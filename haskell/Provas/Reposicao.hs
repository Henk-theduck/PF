{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

-- ======================= |AFORTUNADO| ======================
-- Retorna se um numero é primo ou não

auxPrimo :: Int->Int->Bool
auxPrimo n d
        | d == 1 = True
        | mod n d == 0 = False
        | otherwise = auxPrimo n (d-1)
primo :: Int -> Bool
primo 1 = False
primo n = auxPrimo n (n-1)

-- Retorna o numero primo no indice N
auxCont :: Int->Int->Int
auxCont n p
        | n==0 = p-1
        |primo p = auxCont (n-1) (p+1)
        |otherwise = auxCont n (p+1)
nPrimo :: Int -> Int
nPrimo n = auxCont n 2

--Produto dos n primos
prodPrim :: Int -> Int
prodPrim n = product([nPrimo e | e <- [1..n]])

--Proximo primo
proxPrim :: Int -> Int
proxPrim x
        |not (primo x) = proxPrim (x+1)
        |otherwise = x
-- Afortunado 
afortunado :: Int -> Int
afortunado n = proxPrim ((prodPrim n) +2) - prodPrim n

-- ======================= |GET BY INDEX| ======================
--getByIndex
getIrregularIndex :: Int -> String -> Char
getIrregularIndex n string
                |n `notElem` [0..length string-1] = '*'
                |otherwise = string !! n

getByIndex :: [Int] -> String -> String
getByIndex [] [] = []
getByIndex n [] = []
getByIndex [] string = []
getByIndex numbers string = [ getIrregularIndex n string | n <- numbers]

-- ======================= |TRIANGULO DE PASCAL| ======================
--Triangulo de Pascal
pascal :: Int -> [(Int, Int)]
pascal 0 = [(0,0)]
pascal n = [(n, e) | e <- [0 .. n]]

