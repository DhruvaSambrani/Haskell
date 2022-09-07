auxlengthmatch :: Int -> Int -> Int
auxlengthmatch a b = if a == b then a else -1

isMatrix :: [[a]] -> Bool
isMatrix [] = False
isMatrix a = reduced_length /= 0 && reduced_length /= -1
  where
    reduced_length = foldr (auxlengthmatch . length) (length (head a)) a

size :: [[a]] -> (Int, Int)
size a = (length a, length (head a))

isSquareMatrix :: [[a]] -> Bool
isSquareMatrix [] = False
isSquareMatrix a = isMatrix a && l == w
  where
    (l, w) = size a

addable :: [[a]] -> [[a]] -> Bool
addable a b = isMatrix a && isMatrix b && (size a == size b)

addMatrices :: Num a => [[a]] -> [[a]] -> [[a]]
addMatrices = zipWith (zipWith (+))

multiplyable :: [[a]] -> [[a]] -> Bool
multiplyable a b = isMatrix a && isMatrix b && w1 == l2
  where
    (l1, w1) = size a
    (l2, w2) = size b

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose a = map head a : transpose (map tail a)

auxmultiplyMatrices :: Num a => [[a]] -> [[a]] -> [[a]]
auxmultiplyMatrices a b = map g a
  where
    g a = map f b
      where
        f x = sum (zipWith (*) a x)

multiplyMatrices :: Num a => [[a]] -> [[a]] -> [[a]]
multiplyMatrices a b = auxmultiplyMatrices a (transpose b)
