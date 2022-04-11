{-  Si scriva un predicato lowertriangular che determina se una matrice (quadrata) è triangolare
    inferiore.
    A titolo di esempio, lowertriangular([[1,0,0],[2,-3,0],[4,5,6]]) restituisce True, mentre
    lowertriangular([[0,0,1],[2,-3,0],[4,5,6]]) restituisce False. -}
main = do
  print (low [[1,2,3],[4,5,6],[8,9,0]])
  print (low [[1,0,0],[2,-3,0],[4,5,6]])
  print (low [[1,2,1],[0,-3,2],[0,0,6]])
  print (check [1,0,0])
  print (check [0,0,0])

low :: [[Integer]] -> Bool
low (x:xs) = low2 (colsums (x:xs))

colsums :: [[Integer]] -> [[Integer]]
colsums ([]:_) = []
colsums (x:xs) = work (x:xs) : (colsums (map tail (x:xs)))

work :: [[Integer]] -> [Integer]
work [] = []
work (x:xs) = let (f:fs) = x in f : work xs

low2 :: [[Integer]] -> Bool
low2 [] = True
low2 (x:xs)
   | (check (tail aa)) == True = low2 (remCl xs)
   | (check aa) == False = False
 where {
     aa = let (f:fs) = (x:xs) in f;
 }

check :: [Integer] -> Bool
check [] = True
check (x:xs) 
   | x == 0 = check xs
   | x /= 0 = False

remCl :: [[Integer]] -> [[Integer]]
remCl [] = []
remCl (x:xs) = a : remCl xs
 where {
    a = let (f:fs) = x in fs;
 }

{- [[1,2,3],[4,5,6],[8,9,0]] -}