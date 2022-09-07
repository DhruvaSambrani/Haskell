subWord :: String -> String -> Bool
subWord [] _ = True
subWord _ [] = False
subWord needle haystack
  | needle == take (length needle) haystack = True
  | otherwise = subWord needle (tail haystack)
