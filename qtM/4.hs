data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   let z = (Q (C 1) (Q (C 0) (C 0) (C 0) (C 0)) (C 0) (C 1))
   let y = (Q (Q (C 0) (C 0) (C 0) (C 0)) (Q (C 0) (C 0) (C 0) (C 0)) (C 0) (C 1))
   let mx = Mat 2 z {- (Q z (C 0) (C 0) z) -}
   print (Mat 2 (sumMat z y))


{- Ma qua devo usare le matrici vero? -}

keep Mat{nexp=n, mat=(C x)} = x
sumMat :: (Num a, Eq a, Show a) => QT a -> QT a -> QT a
sumMat (C x) (C y) = C (keep (Mat 0 (C x)) + keep (Mat 0 (C y)))
sumMat (C x) (Q y1 y2 y3 y4) = (Q (sumMat (C x) y1) (sumMat (C x) y2) (sumMat (C x) y3) (sumMat (C x) y4))
sumMat (Q x1 x2 x3 x4) (C y) = (Q (sumMat x1 (C y)) (sumMat x2 (C y)) (sumMat x3 (C y)) (sumMat x4 (C y)))
sumMat (Q x1 x2 x3 x4) (Q y1 y2 y3 y4) = (Q (sumMat x1 y1) (sumMat x2 y2) (sumMat x3 y3) (sumMat x4 y4))































{- Somma tra matrici 

check :: (Num a, Eq a, Show a) => Mat a -> Mat a -> QT a
check (Mat n (Q x1 x2 x3 x4)) (Mat n1 (Q z1 z2 z3 z4)) = if (n>1) then (Q f1 f2 f3 f4) else (Q (C y1) (C y2) (C y3) (C y4))
  where {
      f1 = (check (Mat (n-1) x1) (Mat (n1-1) z1));
      f2 = (check (Mat (n-1) x2) (Mat (n1-1) z2)); 
      f3 = (check (Mat (n-1) x3) (Mat (n1-1) z3));
      f4 = (check (Mat (n-1) x4) (Mat (n1-1) z4));
      y1 = keep (Mat 0 x1) + keep (Mat 0 z1);
      y2 = keep (Mat 0 x2) + keep (Mat 0 z2);
      y3 = keep (Mat 0 x3) + keep (Mat 0 z3);
      y4 = keep (Mat 0 x4) + keep (Mat 0 z4);
  }
-}