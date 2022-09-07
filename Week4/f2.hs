auxf :: [Int] -> Int -> [Char] -> [Char]
auxf [] _ ls = ls
auxf (x:xs) n ls = auxf xs (n+1) (ls ++ [l]) where
    l = if x > n then 'a' else 'b'

f2 :: [Int] -> [Char]
f2 ns = auxf ns 0 [] 

