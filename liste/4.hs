{- Scrivere una funzione che calcola i 2 minori elementi dispari di una lista (se esistono).
 Ad esempio minOdd([2,3,4,6,8,7,5]) riduce a (3,5) -}
minOdd :: [Integer] -> (Integer,Integer)
minOdd [] = (1,-1)
minOdd (x:xs) = let (f:fs) = a (x:xs) in (f,head fs)

a :: [Integer] -> [Integer]
a [] = []
a (x:xs) 
  | mod x 2 /= 0 = work x (xs) ++ a xs ++ [x]
  | mod x 2 == 0 = a xs ++ [x]
 

work :: Integer -> [Integer] -> [Integer]
work v (x:xs)
  | v <= x = work v xs 
  | v > x = []
work v [] = [v]


{- Altra versione -}
qs :: [Integer] -> [Integer]
qs ([]) = []
qs (x:xs) = qs(minori x xs) ++ [x] ++ qs(maggiori x xs)

minori :: Integer -> [Integer] -> [Integer]   {- metto uno dei due -}
minori p [] = [] 
minori p (x:xs)
   | x <= p = x : minori p (xs)
   | x > p = minori p (xs) 

maggiori :: Integer -> [Integer] -> [Integer]    {- metto uno dei due -}
maggiori p [] = [] 
maggiori p (x:xs)
   | x > p = x : maggiori p (xs)
   | x <= p = maggiori p (xs) 
    
minOdd2 :: [Integer] -> (Integer,Integer)
minOdd2 [] = (1,-1)
minOdd2 (x:xs) = let (f:fs) = qs (a2 (x:xs)) in (f,head fs)

a2 [] = []
a2 (x:xs) 
  | mod x 2 /= 0 = a2 xs ++ [x]
  | mod x 2 == 0 = a2 xs 