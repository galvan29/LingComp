{- let y:ys = x in ciao y:ys -} 
main2 = do 
    print (colsums [[1,2,3],[4,5,6]]) 
    print (colsums [[1,2,3],[4,5,6],[7,8,9]]) 
 
colsums :: [[Integer]] -> [Integer]
colsums (x:xs) = let (f:fs) = (changeCol (x:xs)) in (sum f:fs)

changeCol ::  [[Integer]] -> [[Integer]]

<<<<<<< HEAD
=======
{- ciao -}
>>>>>>> c5e35fc8df3cf7b48e6b7ca10d7e58cc1933db6f
