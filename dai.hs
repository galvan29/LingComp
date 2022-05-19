data QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
data Mat a = Mat {nexp :: Int, mat :: QT a}

main = do
   let z = (Q (C 1) (C 0) (C 0) (C 1))
   print (diagonal (Mat 2 (Q z (C 0) (C 0) z)))
   print (diagonal (Mat 2 (Q z z z z)))
   
diagonal :: (Eq a, Num a) => Mat a -> Maybe [a]
diagonal mat = if (dNZero mat) then Just (dToList mat) else Nothing

dNZero :: (Eq a, Num a) => Mat a -> Bool
dNZero Mat{nexp=n, mat=(C x)} = x/=0 || n==0
dNZero (Mat n (Q x1 x2 x3 x4)) = if (n>0) then ((dNZero (Mat (n-1) x1)) && (dNZero (Mat (n-1) x4)) && (uLZero (Mat (n-1) x2)) && (uLZero (Mat (n-1) x3))) else False

uLZero :: (Eq a, Num a) =>  Mat a -> Bool
uLZero (Mat n (C x)) = x==0
uLZero (Mat n (Q x1 x2 x3 x4)) = if (n>0) then ((uLZero (Mat (n-1) x1)) && (uLZero (Mat (n-1) x2)) && (uLZero (Mat (n-1) x3)) && (uLZero (Mat (n-1) x4))) else False

dToList :: Mat a -> [a]
dToList (Mat n (C x)) = [x]
dToList (Mat n (Q x1 x2 x3 x4)) = ((dToList (Mat (n-1) x1)) ++ (dToList (Mat (n-1) x4)))