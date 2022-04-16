data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)

main = do
   let z = C 0; u = C 1; q = Q z u u u
   print (occurrencies (Q q (C 0) (C 2) q) 0)

occurrencies :: (Eq a, Num a, Ord a, Integral a) => QT a -> a -> a
occurrencies (Q a b c d) v = occurrencies2 (Q a b c d) v ((howManyPixels (Q a b c d)))

occurrencies2 :: (Eq a, Num a, Ord a, Integral a) => QT a -> a -> a -> a
occurrencies2 (C k) v liv = if(k == v) then 4^(liv) else 0
occurrencies2 (Q a b c d) v liv = (occurrencies2 a v (liv-1)) + (occurrencies2 b v (liv-1)) + (occurrencies2 c v (liv-1)) + (occurrencies2 d v (liv-1))

howManyPixels :: (Integral a, Num a, Ord a, Eq a) => QT a -> a
howManyPixels (C r) = 1
howManyPixels (Q d e f g) = work
 where {
     work = fromIntegral (max2 [(howManyPixels2 d), (howManyPixels2 e), (howManyPixels2 f), (howManyPixels2 g)]);
 }

max2 :: (Num a, Ord a, Eq a) => [a] -> a
max2 [x] = x
max2 (x:x':xs) | x >= x'   = max2 (x:xs)
max2 (x:x':xs) | otherwise = max2 (x':xs)

howManyPixels2 :: (Num a, Ord a, Eq a) => QT a -> a
howManyPixels2 (C r) = 1
howManyPixels2 (Q d e f g) = 1 + howManyPixels2 d 

