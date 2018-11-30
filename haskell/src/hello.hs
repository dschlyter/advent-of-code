module Hello where
    bang :: [Char] -> [Char]
    bang x = x ++ "!"
    
    fifth :: [Char] -> Char
    fifth = (head . drop 4)
    
    d9 :: [Char] -> [Char]
    d9 = drop 9
    
    thirdLetter :: [Char] -> Char
    thirdLetter x = x !! 2

    lala x = bang x