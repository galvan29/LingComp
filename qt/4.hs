{- 4. Si scriva una funzione howManyPixels che dato un QuadTree determina il numero (minimo) di
   pixel di quell’immagine. Ad esempio
   let z = C 0; u = C 1; q = Q z u u u in howManyPixels ( Q q ( C 0) ( C 2) q ) estituisce 16. -}

data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)

main = do
    let z = C 0 
    let u = C 1
    let q = Q z u u u
    print (howManyPixels ( Q q ( C 0) ( C 2) q ))
    print (howManyPixels (Q (Q (C 0) (C 1) (C 1) (C 1)) (C 0) (C 2) (Q (C 0) (C 1) (C 1) (C 1))))

howManyPixels :: (Integral a, Num a, Ord a, Eq a) => QT a -> a
howManyPixels (C r) = -1
howManyPixels (Q d e f g) = 2^(ceiling (sqrt work))
 where {
     work = fromIntegral (howManyPixels2 (Q d e f g));
 }

howManyPixels2 :: (Num a, Ord a, Eq a) => QT a -> a
howManyPixels2 (C r) = 1
howManyPixels2 (Q d e f g) = howManyPixels2 d + howManyPixels2 e + howManyPixels2 f + howManyPixels2 g