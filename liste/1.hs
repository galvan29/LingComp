remove :: [Integer] -> [Integer]

remove [] = []
remove (x:[]) = []
remove (x:y:xs) = y : remove(xs)

{- remove [1, 2, 3, 4, 5, 6, 7, 8, 9] -}