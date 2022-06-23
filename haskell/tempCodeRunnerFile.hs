
-- convertSeconds n
--             | n < 59 = (0,0,n)
--             | otherwise = ((n/3600),((n/60)-60),(((n/60)-60)/60))