-- exemplo: considere uma função que retorne a quantidade de vendas de um a empresa na semana
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
venda::Int->Int
venda 0 = 5
venda 1 = 6
venda 2 = 4
venda 3 = 8

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

-- maiorSemana :: Int -> Int 
-- maiorSemana x
--         |