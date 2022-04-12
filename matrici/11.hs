main = do
    print (moltipl [[1,0,1],[2,3,4]] [[1,2],[4,1],[0,1]])
    
moltipl :: [[Integer]] -> [[Integer]] -> [[Integer]]
moltipl x y = b x (colsums y)

b :: [[Integer]] -> [[Integer]] -> [[Integer]]
b [] y = []
b (x:xs) (y:ys) = c x y : b xs y

c :: [Integer] -> [[Integer]] -> [Integer]
c (x:xs) [] = []
c (x:xs) (y:ys) = (somma x y) : c x ys

somma :: [Integer] -> [Integer] -> Integer
somma (x:[]) (y:[]) = x * y
somma (x:xs) (y:ys) = x*y + somma xs ys

colsums :: [[Integer]] -> [[Integer]]
colsums ([]:_) = []
colsums x = work x : (colsums (map tail x)

work :: [[Integer]] -> [Integer]
work [] = []
work (x:xs) = let (f:fs) = x in f : work xs