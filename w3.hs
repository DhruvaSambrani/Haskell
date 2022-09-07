f = length (foldr f [0] [0..10]) where 
        f x y = if even x  
        then x: x + head y : y
        else x: y
