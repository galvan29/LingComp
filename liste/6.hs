main = do
    print (coppie [1,2,3,4])

coppie :: [Integer] -> [(Integer,Integer)]
coppie [] = [(1,-1)]
coppie (x:xs) = reverse (coppie2 (reverse ([x] ++ xs)))

coppie2 :: [Integer] -> [(Integer,Integer)]
coppie2 [] = []
coppie2 (x:xs) = [(x, sum xs)] ++ coppie2 xs 