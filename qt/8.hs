{- 8. Si scriva una funzione Haskell overColor che dato un colore c ed un QuadTree q determina il
numero (minimo) di pixel dell’immagine codificata da q che hanno un colore maggiore di c. Ad
esempio
let d = C 2; u = C 1; q = Q d u u u
in overColor 1 ( Q q ( C 0) ( C 3) q )
restituisce 6 (visto che il QuadTree codifica almeno 16 pixel). -}


data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)

main = do
   let d = C 2; u = C 1; q = Q d u u u
   let f = (Q q (C 0) (C 3) q)
   print (difference 1 f)

difference :: (Eq a, Num a, Ord a, Integral a) => a -> QT a -> a
difference val (Q a b c d) = (occurrencies (Q a b c d) val)

occurrencies :: (Eq a, Num a, Ord a, Integral a) => QT a -> a -> a
occurrencies (Q a b c d) v = occurrencies2 (Q a b c d) v ((howManyPixels (Q a b c d)))

occurrencies2 :: (Eq a, Num a, Ord a, Integral a) => QT a -> a -> a -> a
occurrencies2 (C k) v liv = if(k > v) then 4^(liv) else 0
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
