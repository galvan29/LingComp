main = do
    print (convergent [[1,2,3],[4,5,6],[7,8,9]] 20)
    print (convergent [[1,2,3],[4,5,6],[7,8,9]] 15)
    
convergent :: [[Integer]] -> Integer -> Bool
convergent [] = []
convergent (x:xs) r = if (foldl (\z y -> if (sum h) > r then 0 else 1) 0 (xs)) == 0 then 
convergent (xs) else False
 