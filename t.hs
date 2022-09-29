prefix [] = []
prefix seq = map (`take` seq) [4..(length seq - 1)]

isbalanced :: String -> Bool
isbalanced seq 
    | length seq == 4 = seq `elem` ["bbaa", "baab", "abab", "baba", "aabb", "abba"]
    | otherwise = all isbalanced (prefix seq)

balanced :: Int -> [String]
balanced 4 = ["bbaa", "baab", "abab", "baba", "aabb", "abba"]
balanced n = filter (""/=) (concatMap extendseq (balanced (n-1)))
    where
        extendseq seq =  extendseqa seq ++ extendseqb seq
        extendseqa seq = if isbalanced (seq ++ "a") then [seq ++ "a"] else [[]]
        extendseqb seq = if isbalanced (seq ++ "b") then [seq ++ "b"] else [[]]
