type Hora = (Int, Int, Int)

validateHour :: Hora -> Bool
validateHour (h,m,s)
        | s < 0 || s > 59 = False
        | m < 0 || m > 59 = False
        | h < 0 || h > 23 = False
        |otherwise = True

convertHour :: Hora -> Int
convertHour (h,m,s)
            | not(validateHour(h,m,s)) = 0
            | otherwise = (h*3600) + (m*60) + s


returnHour :: Int -> Int
returnHour n = div n 3600

returnMinutes :: Int -> Int
returnMinutes n = subtract (div n 60) (returnHour n)

-- convertSeconds :: Int -> Hora 
-- convertSeconds n
--             | n < 59 = (0,0,n)
--             | otherwise =