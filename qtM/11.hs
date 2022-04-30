data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   let w2 = (Q (C 1) (C 3) (C 1) (C 1))
   let w0 = (Q (C 1) (C 3) (C 0) (C 4))
   let w1 = (Q (C 1) (C 0) (C 0) (C 2))
   let ww = (Q w1 (Q (C 1) (C 0) (C 3) (C 7)) w0 (Q (C 3) (C 1) (C 1) (C 0)))
   let mat3 = Mat 2 ww
   print (searchMinMax mat3)

searchMax :: (Eq a, Show a, Num a, Ord a) => Mat a -> [a]
searchMax m = csum (mat m)
    where
        n = nexp m
        csum (C c)       = take (2 ^ n) $ repeat (c * 2 ^ n)
        csum (Q a b c d) = zipWith (\x y -> if(x>y) then x else y) ((searchMax $ submat a) ++ (searchMax $ submat b))
                                       ((searchMax $ submat c) ++ (searchMax $ submat d))
        submat q = Mat (n - 1) q

searchMin :: (Eq a, Show a, Num a, Ord a) => Mat a -> [a]
searchMin m = csum (mat m)
    where
        n = nexp m
        csum (C c)       = take (2 ^ n) $ repeat (c * 2 ^ n)
        csum (Q a b c d) = zipWith (\x y -> if(x<y) then x else y) ((searchMin $ submat a) ++ (searchMin $ submat b))
                                       ((searchMin $ submat c) ++ (searchMin $ submat d))
        submat q = Mat (n - 1) q

searchMinMax :: (Eq a, Num a, Show a, Ord a) => Mat a -> [a]
searchMinMax mat = createCouple (searchMin mat) (searchMax mat)

createCouple :: (Eq a, Show a, Num a, Ord a) => [a] -> [a] ->[a]
createCouple (x:[]) (y:[]) = [y-x]
createCouple (x:xs) (y:ys) = (y-x) : createCouple xs ys