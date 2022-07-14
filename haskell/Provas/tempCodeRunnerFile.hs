dena :: String -> Char
-- ordena (a:b:x)
--         |null x = a
--         |b == a = ordena 
--         |contElement a x > contElement b x = ordena (a:x)
--         |otherwise = ordena (b:x)

-- ordenaFreq :: String -> String
-- ordenaFreq (a:x)
--             | null x = []
--             |otherwise = ordena (a:x) : ordenaFreq x