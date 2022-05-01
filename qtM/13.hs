data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   let w2 = (Q (C 1) (C 3) (C 1) (C 1))
   let w0 = (Q (C 1) (C 3) (C 0) (C 4))
   let w1 = (Q (C 1) (C 0) (C 0) (C 2))
   let ww = (Q w1 (Q (C 1) (C 0) (C 3) (C 7)) w0 (Q (C 3) (C 1) (C 1) (C 0)))
   let mat3 = Mat 2 ww
   print (transpose mat3)


transpose2 :: (Eq a, Show a, Num a, Ord a) => Mat a -> [a]
transpose2 m = csum (mat m)
    where
        n = nexp m
        csum (C c)       = take (2 ^ n) $ repeat (c * 2 ^ n)
        csum (Q a b c d) = zipWith (\x y -> x)  ((transpose $ submat a) ++ (transpose $ submat c))                                     ((transpose $ submat b) ++ (transpose $ submat d))
        submat q = Mat (n - 1) q

transpose :: (Eq a, Show a, Num a, Ord a) => Mat a -> Mat a
transpose Mat n (C x) = Mat n (C x)
transpose Mat n (Q a b c d) = Mat n (transpose (Mat (n-1) a)) (transpose (Mat (n-1) b)) (transpose (Mat (n-1) c)) (transpose (Mat (n-1) d))
        