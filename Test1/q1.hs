f1 :: [a] -> Int -> [a]
f1 l 0 = l
f1 l 1 = l
f1 [] n = []
f1 l n = take (n-1) l ++ f1 (drop n l) n

auxf2 :: [a] -> [[a]] -> [a] -> [[a]]
auxf2 [] acc _ = acc
auxf2 (x:xs) acc lastseq = auxf2 xs (acc ++ [newls]) newls
    where
    newls = lastseq ++ [x]

f2 :: [a] -> [[a]]
f2 l = auxf2 l [[]] []


auxf3 :: [a] -> [[a]] -> [[a]] -> [[a]]
auxf3 [] acc lastseqs = acc
auxf3 (x:xs) acc lastseqs = auxf3 xs (acc ++ nlastseqs) (nlastseqs++[[]])
    where
    nlastseqs = map (\l -> l ++ [x]) lastseqs

f3 :: [a] -> [[a]]
f3 l = auxf3 l [[]] [[]]
