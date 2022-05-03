data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   let w2 = (Q (C 1) (C 3) (C 1) (C 1))
   let w0 = (Q (C 1) (C 3) (C 0) (C 4))
   let w1 = (Q (C 1) (C 0) (C 0) (C 2))
   let ww = (Q w1 (Q (C 1) (C 0) (C 3) (C 7)) w0 (Q (C 3) (C 1) (C 1) (C 0)))
   let mat3 = Mat 2 ww
   print (colAltSum mat3)

colAltSum :: (Eq a, Show a, Num a, Ord a) => Mat a -> [a]
colAltSum (Mat n (Q a b c d))= work (convert  (Q a b c d))

work :: (Eq a, Show a, Num a, Ord a) => QT a -> Int -> [a]
work (C x) = [x]
work (Q a b c d) n = if(n==1) then zipWith (-) [a,b] [c,d] else zipWith (+) ((work $ submat a) ++ (work $ submat b)) ((work $ submat c) ++ (work $ submat d))
 where
   submat q = Mat (n - 1) q


convert :: (Num a, Eq a, Show a, Ord a) => Int -> QT a -> QT a
convert n (C x) = if(n==1) then (Q (C x) (C x) (C x) (C x)) else (Q (convert (n-1) (C x)) (convert (n-1) (C x)) (convert (n-1) (C x)) (convert (n-1) (C x)))
convert n (Q x1 x2 x3 x4) = if(n==1) then (Q x1 x2 x3 x4) else (Q (convert (n-1) x1) (convert (n-1) x2) (convert (n-1) x3) (convert (n-1) x4))
