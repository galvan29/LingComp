data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   let w2 = (Q (C 1) (C 3) (C 1) (C 1))
   let w0 = (Q (C 1) (C 3) (C 0) (C 4))
   let w1 = (Q (C 1) (C 0) (C 0) (C 2))
   let ww = (Q w1 (Q (C 1) (C 0) (C 3) (C 7)) w0 (Q (C 3) (C 1) (C 1) (C 0)))
   let mat3 = Mat 2 ww
   print (transpose ww)

transpose :: (Eq a, Show a, Num a, Ord a) => QT a -> QT a
transpose (C x) = (C x)
transpose (Q a b c d) = (Q (transpose a) (transpose b) (transpose c) (transpose d))
        