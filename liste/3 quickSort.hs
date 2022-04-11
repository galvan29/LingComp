qs :: [Integer] -> [Integer]
qs ([]) = []
qs (x:xs) = qs(minori x xs) ++ [x] ++ qs(maggiori x xs)

minori :: Integer -> [Integer] -> [Integer]   {- metto uno dei due -}
minori :: (Ord a) => [a]->[a]
minori p [] = [] 
minori p (x:xs)
   | x <= p = x : minori p (xs)
   | x > p = minori p (xs) 

maggiori :: Integer -> [Integer] -> [Integer]    {- metto uno dei due -}
maggiori :: (Ord a) => [a]->[a]
maggiori p [] = [] 
maggiori p (x:xs)
   | x > p = x : maggiori p (xs)
   | x <= p = maggiori p (xs) 
    