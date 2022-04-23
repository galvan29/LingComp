data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   let z = (Q (C 1) (C 0) (C 0) (C 1))
   print (lowertriangular (Mat 2 (Q z (C 2) (C 0) z)))