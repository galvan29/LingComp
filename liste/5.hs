coppie :: [Integer] -> [(Integer,Integer)]
coppie [] = []
coppie (x:xs) = [(x,sum xs)] ++ coppie (xs)