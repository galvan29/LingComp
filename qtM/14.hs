data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   let w2 = (Q (C 1) (C 3) (C 1) (C 1))
   let w0 = (Q (C 1) (C 3) (C 0) (C 4))
   let w1 = (Q (C 1) (C 0) (C 0) (C 2))
   let ww = (Q w1 (Q (C 1) (C 0) (C 3) (C 4)) w0 (Q (C 3) (C 1) (C 1) (C 0)))
   let mat3 = Mat 2 ww
   print (colAltSum (transpose mat3))
   
isSymmetric :: (Eq a, Show a, Num a, Ord a) => Mat a -> Bool
isSymmetric mat = check (transpose mat) (notTranspose mat)


transpose :: (Eq a, Show a, Num a, Ord a) => Mat a -> [a]
transpose m = csum (mat m)
    where
        n = nexp m
        csum (C c)       = take (2 ^ n) $ repeat (c * 2 ^ n)
        csum (Q a b c d) = ((transpose $ submat a) ++ (transpose $ submat c)) ++
                                       ((transpose $ submat b) ++ (transpose $ submat d))
        submat q = Mat (n - 1) q

notTranspose :: (Eq a, Show a, Num a, Ord a) => Mat a -> [a]
notTranspose m = csum (mat m)
    where
        n = nexp m
        csum (C c)       = take (2 ^ n) $ repeat (c * 2 ^ n)
        csum (Q a b c d) = ((notTranspose $ submat a) ++ (notTranspose $ submat b)) ++
                                       ((notTranspose $ submat c) ++ (notTranspose $ submat d))
        submat q = Mat (n - 1) q

check :: (Eq a, Show a, Num a, Ord a) => [a] -> [a] -> Bool
check (x:[]) (y:[]) = if(x==y) then True else False
check (x:xs) (y:ys) = if(x==y) then colAltSum xs ys else False
