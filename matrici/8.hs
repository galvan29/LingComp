main = do
    print (convergent [[1,2,3],[4,5,6],[7,8,9]] 30)
    print (convergent [[1,2,3],[4,5,6],[7,8,9]] 15)
{-    print ((sum (h * h | h <- [1,2,3])))   -}
    print [ x * x | x <- [2,4..18]]
    
convergent :: [[Integer]] -> Integer -> Bool
convergent (x:xs) r = checkSum (x:xs) r
 
checkSum :: [[Integer]] -> Integer -> Bool
checkSum [] r = True
checkSum (x:xs) r = if (foldl (\z y -> if (abs (sum x)) < r then 0 else 1) 0 (xs)) == 0 then (checkSum (xs) r) else False