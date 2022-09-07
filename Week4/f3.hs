f3 :: [Int] -> [Int]
f3 [] = []
f3 [x] = [x]
f3 (x:y:xs)
    | x == y = f3 (x:xs)
    | otherwise = x : f3 (y:xs)
