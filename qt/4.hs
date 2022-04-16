{- 4. Si scriva una funzione howManyPixels che dato un QuadTree determina il numero (minimo) di
   pixel di quell’immagine. Ad esempio
   let z = C 0; u = C 1; q = Q z u u u in howManyPixels ( Q q ( C 0) ( C 2) q ) estituisce 16. -}

data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)

main = do
    let u = Q (C 0) (C 0) (C 0) (C 0)
    let z = Q u u u u 
    let q = Q z u u u
    print (howManyPixels (Q q (C 0) (C 2) q))

howManyPixels :: (Integral a, Num a, Ord a, Eq a) => QT a -> a
howManyPixels (C r) = 1
howManyPixels (Q d e f g) = 4^work
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