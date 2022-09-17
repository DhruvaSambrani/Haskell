parseInt :: String -> Int
parseInt "end" = 0
parseInt "sum" = 0
parseInt "zero" = 0
parseInt "one" = 1
parseInt "two" = 2
parseInt "three" = 3
parseInt "four" = 4
parseInt "five" = 5
parseInt "six" = 6
parseInt "seven" = 7
parseInt "eight" = 8
parseInt "nine" = 9

getandsum old = do {
    ; newstr <- getLine
    ; let new = parseInt newstr
    ; if newstr=="end" then do {
        putStr ("The final sum is: " ++ show (old+new))
        ; return ()
    } else if newstr == "sum" then do {
        putStrLn ("The current sum is: " ++ show old)
        ; getandsum (old+new)
    } else getandsum (old+new)
}

main = getandsum 0
