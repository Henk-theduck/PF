
-- ======================= Função Primo ========================
-- Retorna se um numero é primo ou não
auxPrimo :: Int->Int->Bool
auxPrimo n d
        | d == 1 = True
        | mod n d == 0 = False
        | otherwise = auxPrimo n (d-1)
primo :: Int -> Bool
primo 1 = False
primo n = auxPrimo n (n-1)

-- ======================= Função N - Primo ===================
-- Retorna o numero primo no indice N
auxCont :: Int->Int->Int
auxCont n p
        | n==0 = p-1
        |primo p = auxCont (n-1) (p+1)
        |otherwise = auxCont n (p+1)
nPrimo :: Int -> Int
nPrimo n = auxCont n 2

-- ========================= Função Primo Gemeos ==================
-- Retorna se dois numeros são primos gemeos -> diferença entre os dois numeros igual a 2
primoGemeos :: Int -> Int -> Bool
primoGemeos m n
            | not (primo m) || not (primo n) = False
            | n < m = primoGemeos n m
            | n - m == 2 = True
            | otherwise = False

-- ============================= Função ehPrimo =======================
-- Retorna se o numero é Primo Gemeo com algum outro
auxGemeo :: Int -> Int -> Bool
auxGemeo n i
            |nPrimo i > n+2 = False
            |primoGemeos (nPrimo i) n = True
            |otherwise = auxGemeo n (i+1)

ehPrimoGemeo :: Int -> Bool
ehPrimoGemeo n
            | not (primo n) = False
            | otherwise =  auxGemeo n 1

-- ========================== Funçao ContGemeo ==========================================
-- Retorna a quantidade de pares primos gemeos abaixo de um numero
auxContGemeo :: Num p => Int -> Int -> Int -> p
auxContGemeo n i j
                | nPrimo j > n = 0
                | primoGemeos (nPrimo i) (nPrimo j) = 1+auxContGemeo n (i+1) (j+1)
                | otherwise = auxContGemeo n (i+1) (j+1)

contParGemeo :: Num p => Int -> p
contParGemeo n = auxContGemeo n 1 2

-- =======================  Função Soma Gemeos ================================
-- Retorna a soma de todos os pares de gemeos abaixo de um numero

auxSomaGemeos :: Int -> Int -> Int -> Int
auxSomaGemeos n i j
                    | nPrimo j > n = 0
                    | primoGemeos (nPrimo i) (nPrimo j) = nPrimo i + nPrimo j + auxSomaGemeos n (i+1) (j+1)
                    | otherwise = auxSomaGemeos n (i+1) (j+1)

somaGemeos :: Int -> Int
somaGemeos n = auxSomaGemeos n 2 3


