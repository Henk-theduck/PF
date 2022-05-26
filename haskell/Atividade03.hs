import System.Win32 (COORD(x))
import Control.Monad.Cont (cont)
-- Questão 1 potencia
pot :: Int -> Int -> Int
pot m n
    | n == 0 = 1
    | n == 1 = m
    | otherwise = m * pot m (n-1)

-- Questão 2 retorna primo no indice n
auxPrimo :: Int->Int->Bool
auxPrimo n d
        | d == 1 = True
        | mod n d == 0 = False
        | otherwise = auxPrimo n (d-1)
primo :: Int -> Bool
primo 1 = False
primo n = auxPrimo n (n-1)

auxCont :: Int->Int->Int
auxCont n p
        | n==0 = p-1
        |primo p = auxCont (n-1) (p+1)
        |otherwise = auxCont n (p+1)
nPrimo :: Int -> Int
nPrimo n = auxCont n 2

-- Questão 3 retorna o primo de fibonacci no indice n
fib :: Int -> Int
fib m
    | m == 0 = 0
    | m == 1 = 1
    |otherwise  = fib (m-1) + fib (m-2)

auxFib :: Int -> Int -> Int
auxFib m n
        | m == 1 = fib n
        |primo(fib n) = auxFib (m-1) (n+1)
        |otherwise = auxFib m (n+1)


fibPrimo :: Int -> Int
fibPrimo m = auxFib m 3

-- Questão 4 - funçõoes com inteiros positivos
-- Calcular o resto da divisao de 2 numeros inteiros positivos
resto :: Int -> Int -> Int
resto m n
        | m < 0 || n < 0 = 0
        | n > m = m
        | otherwise = resto (m-n) n

-- Calcular a divisao inteira entre dois numeros positivos
divisao :: Int -> Int -> Int
divisao m n
        | m < 0 || n < 0 = 0
        | n > m = m
        | otherwise = 1 + divisao (m-n) n
-- retornar o maximo divisor comum entre dois numeros positivos
auxMdc :: Int -> Int -> Int -> Int
auxMdc m n p
        | m == 1 && n == 1 = 1
        | mod m p == 0 && mod n p == 0 = p * auxMdc (div m p) (div n p) p
        | mod m p == 0 && mod n p /= 0 = auxMdc (div m p) n p
        | mod m p /= 0 && mod n p == 0 = auxMdc m (div n p) p
        | otherwise = auxMdc m n (p+1)


mdc :: Int->Int ->Int
mdc m n
        |m < 0 || n < 0 = 0
        | m > n = mdc n m
        | m+1 == n = 1
        | mod n m == 0 = m
        | otherwise =  auxMdc m n 2

dc x y 
        |x < y = dc y x
        | y==0 = x
        | otherwise = dc y (mod x y)

-- retornar o minimo multiplo comum entre dois numeros positivos
auxMmc :: Int -> Int -> Int -> Int
auxMmc m n p
        | m == 1 && n == 1 = 1
        | mod m (nPrimo p) == 0 && mod n (nPrimo p) == 0 = nPrimo p * auxMmc (div m (nPrimo p)) (div n (nPrimo p)) p
        | mod n (nPrimo p) /= 0 = nPrimo p * auxMmc (div m (nPrimo p)) n p
        | mod m (nPrimo p) /= 0 = nPrimo p * auxMmc m (div n (nPrimo p)) p
        | otherwise = auxMmc m n (p+1)

mmc :: Int -> Int -> Int
mmc m n
        |m < 0 || n < 0 = 0
        | m > n = mmc n m
        |primo m && primo n = m*n
        |mod n m == 0 = n
        |otherwise = auxMmc m n 1

--Questao 5  - Informar se o numeros e perfeito ou nao
auxPerfeito :: Int -> Int -> Int
auxPerfeito n cont
                | cont == 1 = 1
                | mod n cont == 0 = cont + auxPerfeito n (cont-1)
                | otherwise = auxPerfeito n (cont-1)
ehPerfeito :: Int -> Bool
ehPerfeito n
        | n < 0 = False
        | n == 1 = False
        | auxPerfeito n (n-1) == n = True
        | otherwise = False