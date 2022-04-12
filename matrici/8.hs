main = do
    print (convergent [[1,2,3],[4,5,6],[7,8,9]] 1000)
    print (convergent [[1,2,3],[4,5,6],[7,8,9]] 10)
{-    print ((sum (h * h | h <- [1,2,3])))   -}
    
convergent :: [[Double]] -> Double -> Bool
convergent (x:xs) r = checkSum (x:xs) r (mainDiagonal (x:xs))
 
checkSum :: [[Double]] -> Double -> [Double] -> Bool
checkSum [] r [] = True
checkSum (x:xs) r (h:hs) = if (sqrt ((sum  [ g * g | g <- x])) - (h*h)) < r then checkSum xs r hs else False


mainDiagonal xs = zipWith (!!) xs [0..]