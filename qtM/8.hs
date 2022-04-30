data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   let w2 = (Q (C 1) (C 3) (C 1) (C 1))
   let w0 = (Q (C 1) (C 3) (C 0) (C 4))
   let w1 = (Q (C 1) (C 0) (C 0) (C 2))
   let ww = (Q w1 (Q (C 1) (C 0) (C 3) (C 4)) w0 (Q (C 3) (C 1) (C 1) (C 0)))
   let mat3 = Mat 2 ww
   print (colsum mat3

colsum :: (Eq a, Show a, Num a) => Mat a -> [a]
colsum m = csum (mat m)
    where
        n = nexp m
        csum (C c)       = take (2 ^ n) $ repeat (c * 2 ^ n)
        csum (Q a b c d) = zipWith (+) ((colsum $ submat a) ++ (colsum $ submat b))
                                       ((colsum $ submat c) ++ (colsum $ submat d))
        submat q = Mat (n - 1) q
