{- 4. Si scriva una funzione colMinMax che, data una matrice implementata come liste di liste per righe,
calcola il vettore delle coppie (minimo,massimo) delle colonne della matrice. -}

main = do 
    print (colMinMax [[1,2,3],[4,5,6],[7,8,9]])

colMinMax :: [[Integer]] -> [(Integer,Integer)]
colMinMax (x:xs) = [(a,b) | (a,b) <- ((y:ys), (z:zs))]
 where { 
  (y:ys) = (map (minore 10) (colsums (x:xs)));
  (z:zs) = (map (maggiore 0) (colsums (x:xs)));
 }

{- (head (map (minore 10) (colsums (x:xs))), head (map (maggiore 0) (colsums (x:xs)))) -}
minore :: Integer -> [Integer] -> Integer   {- metto uno dei due -}
minore p [] = p
minore p (x:xs)
   | x <= p = minore x (xs)
   | x > p = minore p (xs) 

maggiore :: Integer -> [Integer] -> Integer   {- metto uno dei due -}
maggiore p [] = p
maggiore p (x:xs)
   | x > p = maggiore x (xs)
   | x <= p = maggiore p (xs) 





colsums :: [[Integer]] -> [[Integer]]
colsums ([]:_) = []
colsums (x:xs) = work (x:xs) : (colsums (map tail (x:xs)))

work :: [[Integer]] -> [Integer]
work [] = []
work (x:xs) = let (f:fs) = x in f : work xs