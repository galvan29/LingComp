data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   print (zong 3 3 (Mat 1 (Q (C 1) (C 1) (C 1) (C 1))) (Q (C 1) (C 0) (C 1) (C 0)))


zong :: (Num a, Eq a, Show a, Ord a) => a -> a -> Mat a -> QT a -> Mat a
zong x y (Mat n (Q a b c d)) qt = Mat n (menMat (matMul n x (Q a b c d)) (matMul n y qt))

keep Mat{nexp=n, mat=(C x)} = x

matMul :: (Num a, Eq a, Show a, Ord a) => Int -> a -> QT a -> QT a
matMul n val (C x) = C (keep (Mat 0 (C x)) * val)
matMul n val (Q x1 x2 x3 x4) = if (n==1) then (Q a b c d) else (Q e f g h)
 where{
     a = C (keep (Mat 0 x1) * val);
     b = C (keep (Mat 0 x2) * val);
     c = C (keep (Mat 0 x3) * val);
     d = C (keep (Mat 0 x4) * val);
     e = matMul (n-1) val x1;
     f = matMul (n-1) val x2;
     g = matMul (n-1) val x3;
     h = matMul (n-1) val x4;
 }

menMat :: (Num a, Eq a, Show a) => QT a -> QT a -> QT a
menMat (C x) (C y) = C (keep (Mat 0 (C x)) - keep (Mat 0 (C y)))
menMat (C x) (Q y1 y2 y3 y4) = (Q (menMat (C x) y1) (menMat (C x) y2) (menMat (C x) y3) (menMat (C x) y4))
menMat (Q x1 x2 x3 x4) (C y) = (Q (menMat x1 (C y)) (menMat x2 (C y)) (menMat x3 (C y)) (menMat x4 (C y)))
menMat (Q x1 x2 x3 x4) (Q y1 y2 y3 y4) = (Q (menMat x1 y1) (menMat x2 y2) (menMat x3 y3) (menMat x4 y4))
