main = do
   print (isSymmetric [[1,2,3],[1,2,3],[1,2,3]])

isSymmetric :: [[Integer]] -> Bool
isSymmetric (x:xs) = ciao (x:xs) (colsums (x:xs))

ciao :: [[Integer]] -> [[Integer]] -> Bool
ciao [] [] = True
ciao (x:xs) (y:ys) = if ( x == y ) == True then ciao (xs) (ys) else False

colsums :: [[Integer]] -> [[Integer]]
colsums ([]:_) = []
colsums (x:xs) = work (x:xs) : (colsums (map tail (x:xs)))

work :: [[Integer]] -> [Integer]
work [] = []
work (x:xs) = let (f:fs) = x in f : work xs