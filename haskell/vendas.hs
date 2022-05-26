-- exemplo: considere uma função que retorne a quantidade de vendas de um a empresa na semana
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
venda::Int->Int
venda 0 = 5
venda 1 = 6
venda 2 = 0
venda 3 = 8
venda 4 = 5

-- questoes
-- 1 - Qual o total de vendas?
-- 2 - Qual a maior venda?
-- 3 - Em que semana ocorreu a maior venda?
-- 4 - Existiu semana sem venda?

totalVendas :: Int -> Int
totalVendas x
            | x > 3 = totalVendas 3
            | x == 0 = venda 0
            | otherwise = venda x + totalVendas (x-1)

maiorVenda :: Int -> Int 
maiorVenda x
        | x > 3 = maiorVenda 3
        | x == 0 = venda 0
        | venda x > maiorVenda(x-1) = venda x

maiorSemanaVenda :: Int -> Int -> Int
maiorSemanaVenda n res
                | n < 0 = res
                | venda n > venda res = maiorSemanaVenda (n-1) n 
                | otherwise = maiorSemanaVenda (n-1) res

maiorSemana :: Int -> Int 
maiorSemana n = maiorSemanaVenda (n-1) n

semVenda :: Int -> Bool

semVenda n
        | n < 0 = False
        | venda n == 0 = True
        | otherwise = semVenda (n-1)