{- let y:ys = x in ciao y:ys -} 
main2 = do 
    print (ciao [[1,2,3],[3,4]]) 
    print (ciao2 [[1,2,3],[3,4]]) 
 
ciao :: [[Integer]] -> Integer 
ciao (x:xs) = let (y:ys) = x in y 
 
ciao2 :: [[Integer]] -> [Integer] 
ciao2 (x:xs) = let (y:ys) = x in (y:ys)
 
{- Si scriva una funzione matrix_dim che data una matrice ne calcola le dimensioni, se la matrice è 
 ben formata, altrimenti restituisce (-1,-1). -} 
 
main = do 
   print (matrix_dim [[1,2,3],[4,5,6],[7,8]]) 
   print (matrix_dim [[1,2,3],[4,5,6]]) 
   print (matrix_dim [[1,2,3],[4,5,6],[7,8,9]]) 
 
{- matrix_dim :: [[Integer]] -> Integer -} 
matrix_dim rows = let (f:fs) = (check (map length rows) ++ (map length [map length rows])) in (head fs, f)
 
check :: [Int] -> [Int] 
check (x:[]) = [x] 
check (x:y:xs)  
  | y == x = check (y:xs) 
  | y /= x = [-1,-1]