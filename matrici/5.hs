{-  Si scriva un predicato lowertriangular che determina se una matrice (quadrata) è triangolare
    inferiore.
    A titolo di esempio, lowertriangular([[1,0,0],[2,-3,0],[4,5,6]]) restituisce True, mentre
    lowertriangular([[0,0,1],[2,-3,0],[4,5,6]]) restituisce False. -}
main = do
  print (low [[1,2,3],[4,5,6],[8,9,0]])
  print (low [[1,0,0],[2,-3,0],[4,5,6]])
  print (check [1,0,0])
  print (check [0,0,0])



low :: [[Integer]] -> Bool
low [] = True
low (x:xs)
   | (check aa) == True = low (remCl (x:xs))
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