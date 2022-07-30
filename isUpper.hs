import Data.Char
isUpper :: Char -> Bool
isUpper c 
    | ord('A') <= ord(c) && ord(c) <= ord('Z') = True
    | otherwise = False
