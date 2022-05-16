
quantAlg :: Int ->Int
quantAlg x 
        | div x 10 == 0 = 1
        |otherwise = 1+ quantAlg (div x 10)

fat :: Int -> Int
fat x
    | x == 0 = 1
    | otherwise = fat(x-1)*x

auxFat :: Int -> Int -> Int
auxFat x i 
        | x == 1 = i-1
        | otherwise = auxFat (div x i) (i+1)

returnFat :: Int -> Int
returnFat x 
        | x == 1 = 1
        | otherwise = auxFat x 2

-- pg :: Int -> Int -> Int -> Int
-- pg a q n 
--         | n == 1 = a
--         |