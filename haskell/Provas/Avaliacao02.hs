contElement ::Eq a => a -> [a] -> Int
contElement _ [] = 0
contElement letra (x:xs)
            |letra == x = 1 + contElement letra xs
            |otherwise = 0

comprime :: [Char] -> [Char]
comprime [] = []
comprime (a:as)
        |contElement a (a:as) > 0 = a : show (contElement a (a:as)) ++ comprime (drop (contElement a (a:as)) (a:as))
        |otherwise = comprime as


contRepete ::Eq a => [a] -> [(a, Int)]
contRepete [] = []
contRepete (x:xs)
            |contElement x (x:xs) > 0 = ( x, contElement x (x:xs)) : contRepete (drop(contElement x (x:xs)) (x:xs))
            |otherwise =  contRepete xs

-- ordenaRepete::Eq a => [a] -> [(a,Int)]
-- ordenaRepete [] = []
-- ordenaRepete (s:xs) = ordenaRepete [ x| x <- contRepete (s:xs), fst x < s] ++ [s] ++ ordenaRepete [ x | x <- contRepete (s:xs), fst x >= s]