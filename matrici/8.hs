main = do
    print (convergent [[1,2,3],[4,5,6],[7,8,9]] 1000)
    print (convergent [[1,2,3],[4,5,6],[7,8,9]] 15)
{-    print ((sum (h * h | h <- [1,2,3])))   -}
    
convergent :: [[Integer]] -> Integer -> Bool
convergent (x:xs) r = checkSum (x:xs) r
 
checkSum :: [[Integer]] -> Integer -> Bool
checkSum [] r = True
checkSum (x:xs) r = if (foldl (\z y -> if (Floating)(sqrt (sum  [ g * g | g <- y])) < r && z==0 then 0 else 1) 0 (x:xs)) == 0 then True else False