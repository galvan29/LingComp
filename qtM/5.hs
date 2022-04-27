data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   let w2 = (Q (C 2) (C 2) (C 2) (C 2))
   let w0 = (Q (C 0) (C 0) (C 0) (C 0))
   let w1 = (Q (C 1) (C 1) (C 1) (C 1))
   let ww = (Q w1 (C 0) w0 w2)
   let www = (Q w0 w1 w2 w1)
   let z = (Q (C 1) (Q (C 0) (C 0) (C 0) (C 0)) (C 0) (C 1))
   let y = (Q (Q (C 0) (C 0) (C 0) (C 0)) (Q (C 0) (C 0) (C 0) (C 0)) (C 0) (C 1))
   let mx = Mat 2 z {- (Q z (C 0) (C 0) z) -}
   print (Mat 2 (matMul 2 ww www))

keep Mat{nexp=n, mat=(C x)} = x
 
{- Pattern non vanno bene? -} 
matMul :: (Num a, Eq a, Show a, Ord a) => a -> QT a -> QT a -> QT a
matMul n (C x) (C y) = C (keep (Mat 0 (C x)) * keep (Mat 0 (C y)))
matMul n (C x) (Q y1 y2 y3 y4) = (Q (matMul (n-1) (C x) y1) (matMul (n-1) (C x) y2) (matMul (n-1) (C x) y3) (matMul (n-1) (C x) y4))
matMul n (Q x1 x2 x3 x4) (C y) = (Q (matMul (n-1) x1 (C y)) (matMul (n-1) x2 (C y)) (matMul (n-1) x3 (C y)) (matMul (n-1) x4 (C y)))
matMul n (Q x1 x2 x3 x4) (Q y1 y2 y3 y4) = if (n==1) then (Q a b c d) else (Q e f g h)
 where{
     a = C (matMul2 x1 y1 x2 y3);
     b = C (matMul2 x1 y2 x2 y4);
     c = C (matMul2 x3 y1 x4 y3);
     d = C (matMul2 x3 y2 x4 y4);
     e = (matMul3 (matMul (n-1) x1 y1) (matMul (n-1) x2 y3));
     f = (matMul3 (matMul (n-1) x1 y2) (matMul (n-1) x2 y4));
     g = (matMul3 (matMul (n-1) x3 y1) (matMul (n-1) x4 y3));
     h = (matMul3 (matMul (n-1) x3 y2) (matMul (n-1) x4 y4));
 }

matMul2 :: (Num a, Eq a, Show a, Ord a) => QT a -> QT a -> QT a -> QT a -> a
matMul2 (C x) (C y) (C z) (C k) = (keep (Mat 0 (C x)) * keep (Mat 0 (C y)) + (keep (Mat 0 (C z)) * keep (Mat 0 (C k))))

matMul3 :: (Num a, Eq a, Show a, Ord a) => QT a -> QT a -> QT a
matMul3 (Q (C a) (C b) (C c) (C d)) (Q (C e) (C f) (C g) (C h)) = Q (C (a+e)) (C (b+f)) (C (c+g)) (C (d+h))


