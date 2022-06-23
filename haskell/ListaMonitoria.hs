
 -- Lista da monitoria 
 -- Questão 1
 -- Função que retorna se um elemento pertence ou não a uma lista

pertence :: Int -> [Int] -> Bool
pertence num (a:x)
            | num == a = True
            | null x = False
            | otherwise = pertence num x

maior :: [Int] -> Int
maior (a:b:x)
        | null x = a
        | a > b = maior (a:x)
        | otherwise = maior (b:x)
