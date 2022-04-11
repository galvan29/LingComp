{- let y:ys = x in ciao y:ys -} 
main = do 
    print (map sum (colsums [[1,2,3],[4,5,6],[7,8,9]])) 

colsums :: [[Integer]] -> [[Integer]]
colsums ([]:_) = []
colsums (x:xs) = work (x:xs) : (colsums (map tail (x:xs)))

work :: [[Integer]] -> [Integer]
work [] = []
work (x:xs) = let (f:fs) = x in f : work xs