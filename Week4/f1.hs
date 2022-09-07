ispower2 :: Int -> Bool
ispower2 0 = False
ispower2 1 = True
ispower2 n
    | even n = ispower2 (n `div` 2)
    | otherwise = False

auxfunc :: Int -> Int
auxfunc n
    | ispower2 n = 2*n
    | otherwise = 0

f1 :: [Int] -> [Int]
f1 = map auxfunc
