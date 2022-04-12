main = do
    print (moltipl [[1,0,1],[2,3,4]] [[1,2],[4,1],[0,1]])
    
moltipl :: [[Integer]] -> [[Integer]] -> [[Integer]]
moltipl (x:xs) (y:ys) = b (x:xs) (colsums (y:ys))

b :: [[Integer]] -> [[Integer]] -> [[Integer]]
b [] (y:ys) = []
b (x:xs) (y:ys) = c x (y:ys) : b xs (y:ys)

c :: [Integer] -> [[Integer]] -> [Integer]
c (x:xs) [] = []
c (x:xs) (y:ys) = (somma (x:xs) y) : c (x:xs) ys

somma :: [Integer] -> [Integer] -> Integer
somma (x:[]) (y:[]) = x * y
somma (x:xs) (y:ys) = x*y + somma xs ys

colsums :: [[Integer]] -> [[Integer]]
colsums ([]:_) = []
colsums (x:xs) = work (x:xs) : (colsums (map tail (x:xs)))

work :: [[Integer]] -> [Integer]
work [] = []
work (x:xs) = let (f:fs) = x in f : work xs