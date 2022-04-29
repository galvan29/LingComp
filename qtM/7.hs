data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   let w2 = (Q (C 2) (C 2) (C 2) (C 2))
   let w0 = (Q (C 0) (C 0) (C 0) (C 0))
   let w1 = (Q (C 1) (C 1) (C 1) (C 1))
   let ww = (Q (C 0) (C 1) (C 0) (C 0))
{-   print (f [1,2,3,5] (Mat 1 (Q (C 1) (C 1) (C 1) (C 1))))
   print (convert 2 ww)
   print (getRow 2 3 (convert 2 ww))
 -}print (agro (convert 2 ww))

f :: (Num a, Eq a, Show a, Ord a) => [a] -> Mat a -> a
f array (Mat n (Q a b c d)) = 4

convert :: (Num a, Eq a, Show a, Ord a) => Int -> QT a -> QT a
convert n (C x) = if(n==1) then (Q (C x) (C x) (C x) (C x)) else (Q (convert (n-1) (C x)) (convert (n-1) (C x)) (convert (n-1) (C x)) (convert (n-1) (C x)))
convert n (Q x1 x2 x3 x4) = if(n==1) then (Q x1 x2 x3 x4) else (Q (convert (n-1) x1) (convert (n-1) x2) (convert (n-1) x3) (convert (n-1) x4))

createList :: (Num a, Eq a, Show a, Ord a) => QT a -> QT a -> QT a -> QT a -> [[QT a]]
createList a b c d = [[a,b],[c,d]]


createL :: (Num a, Eq a, Show a, Ord a) => QT a -> [[a]]
createL (Q (C a) (C b) (C c) (C d)) = [[a,b,c,d]]
createL (Q a b c d) = createL a ++ createL b ++ createL c ++ createL d

agro :: (Num a, Eq a, Show a, Ord a) => [[a]] -> ([[a]],[[a]])
agro l = splitAt ((length l + 1) `div` 2) l


sumRow :: (Num a, Eq a, Show a, Ord a) => a -> [a] -> a
sumRow s (x:[]) = s*x
sumRow s (x:xs) = s*x+sumRow s (xs)   

getRow :: (Num a, Eq a, Show a, Ord a, Integral a) => Int -> a -> QT a -> a
getRow n s (C x) = s*x
getRow n s (Q a b c d) = (getRow (n-1) s a) + (getRow (n-1) s b) 

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
