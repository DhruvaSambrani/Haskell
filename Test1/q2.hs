auxf1 :: [a] -> [[a]] -> [a] -> [[a]]
auxf1 [] acc _ = acc
auxf1 (x:xs) acc lastseq = auxf1 xs (acc ++ [newls]) newls
    where
    newls = lastseq ++ [x]

f1 :: [a] -> [[a]]
f1 l = auxf1 l [[]] []


auxf2 :: [a] -> [[a]] -> [[a]] -> [[a]]
auxf2 [] acc lastseqs = acc
auxf2 (x:xs) acc lastseqs = auxf2 xs (acc ++ nlastseqs) (nlastseqs++[[]])
    where
    nlastseqs = map (\l -> l ++ [x]) lastseqs

f2 :: [a] -> [[a]]
f2 l = auxf2 l [[]] [[]]

f3 :: String -> String -> Bool
f3 "" _ = True
f3 _ "" = False
f3 (n:needle) (h:haystack) 
    | n == h = f3 needle haystack
    | otherwise = f3 (n:needle) haystack
