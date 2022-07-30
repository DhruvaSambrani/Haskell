mydrop :: Int -> [a] -> [a]
mydrop n l
    | n <= 0 || null l = l
    | otherwise = mydrop (n-1) (tail l)
