data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   let w2 = (Q (C 2) (C 2) (C 2) (C 2))
   let w0 = (Q (C 0) (C 0) (C 0) (C 0))
   let w1 = (Q (C 1) (C 1) (C 1) (C 1))
   let ww = (Q w1 (C 0) w0 w2)
   print (colsum (Mat 2 ww))

colsum :: (Eq a, Show a, Num a) => Mat a -> [a]
colsum m = csum (mat m)
    where
        n = nexp m
        csum (C c)       = take (2 ^ n) $ repeat (c * 2 ^ n)
        csum (Q a b c d) = zipWith (+) ((colsum $ submat a) ++ (colsum $ submat c))
                                       ((colsum $ submat b) ++ (colsum $ submat d))
        submat q = Mat (n - 1) q