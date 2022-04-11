


numeriPrimi n = 1:era [2..n] 
 where
    era [] = []
    era (x:xs)  {- array -}
       | x*x < n = x:era ys
       | otherwise = x:ys
       where
          ys = filter (\y->y `rem` x /= 0) xs


