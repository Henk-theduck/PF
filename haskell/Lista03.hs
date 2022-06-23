--Questão 1
--Letra a - impares de 1 a 100

oddList :: [Int]
oddList = [e | e <- [1..100], odd e]

evenList :: [Int]
evenList = [e | e <- [10..100], even e]

oddListN :: Int -> [Int]
oddListN n = [e | e <- [1..n], odd e]

multList :: Int -> [Int]
multList n = [e | e <- [1..n], mod e 3 == 0, mod e 5 == 0]

quadTupla :: Int -> [(Int,Int)]
quadTupla n = [(p ,p*p)| p <-[1..n]]

tupla3x4 :: [(Int, Int)]
tupla3x4 = [(p, q)| p <- [1..3], q <-[1..4]]

tuplaNxM ::Int -> Int -> [(Int, Int)]
tuplaNxM n m= [(p, q)| p <- [1..n], q <-[1..m]]

fib :: Int -> Int
fib m
    | m == 0 = 0
    | m == 1 = 1
    |otherwise  = fib (m-1) + fib (m-2)

listFib :: Int -> [Int]
listFib n = [fib e | e<-[1..n]]
