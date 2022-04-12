main = do 
    print (trasposta [[1,2,3],[4,5,6],[7,8,9]])
    
trasposta :: [[Integer]] -> [[Integer]]
trasposta (x:xs) = colsums (x:xs)

colsums :: [[Integer]] -> [[Integer]]
colsums ([]:_) = []
colsums (x:xs) = work (x:xs) : (colsums (map tail (x:xs)))

work :: [[Integer]] -> [Integer]
work [] = []
work (x:xs) = let (f:fs) = x in f : work xs