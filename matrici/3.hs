{- let y:ys = x in ciao y:ys -} 
main = do 
    print (colaltsums [[1,2,3],[4,5,6],[7,8,9]])

colaltsums :: [[Integer]] -> [Integer]
colaltsums (x:xs) = map strangeSum (colaltsums2 (x:xs))

colaltsums2 :: [[Integer]] -> [[Integer]]
colaltsums2 ([]:_) = []
colaltsums2 (x:xs) = work (x:xs) : (colaltsums2 (map tail (x:xs)))

work :: [[Integer]] -> [Integer]
work [] = []
work (x:xs) = let (f:fs) = x in f : work xs 

strangeSum :: [Integer] -> Integer
strangeSum (x:[]) = x
strangeSum (x:y:xs) = x - y + strangeSum xs