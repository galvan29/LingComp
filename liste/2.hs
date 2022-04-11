add :: [Integer] -> [Integer]

add [] = []
add (x:[]) = [x]
add (x:y:xs) = x : add (xs)

{- add [1, 2, 3, 4, 5, 6, 7, 8, 9] -}
sum2 :: [Integer] -> Integer
sum2 (x:xs) = sum (add (x:xs))