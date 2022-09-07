subSeq :: String -> String -> Bool
subSeq [] _ = True
subSeq _ [] = False
subSeq needle haystack
  | head needle == head haystack = subSeq (tail needle) (tail haystack)
  | otherwise = subSeq needle (tail haystack)
