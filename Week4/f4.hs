auxf4 :: [Int] -> [[Int]] -> [[Int]]
auxf4 [] upruns = upruns
auxf4 (x:xs) [] = auxf4 xs [[x]]
auxf4 (x:xs) (lastuprun:upruns)
    | x > head lastuprun = auxf4 xs ((x:lastuprun):upruns)
    | otherwise = auxf4 xs ([x]:(lastuprun:upruns))

f4 :: [Int] -> [[Int]]
f4 ls = reverse (map reverse (auxf4 ls []))
