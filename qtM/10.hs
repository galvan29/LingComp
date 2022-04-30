data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   let w2 = (Q (C 2) (C 2) (C 2) (C 2))
   let w0 = (Q (C 0) (C 0) (C 0) (C 0))
   let w1 = (Q (C 1) (C 1) (C 1) (C 1))
   let ww = (Q w1 (C 0) w0 w2)
   print (searchMin (Mat 2 ww))

searchMax :: (Eq a, Show a, Num a, Ord a) => Mat a -> [a]
searchMax m = csum (mat m)
    where
        n = nexp m
        csum (C c)       = take (2 ^ n) $ repeat (c * 2 ^ n)
        csum (Q a b c d) = zipWith (\x y -> if(x>y) then x else y) ((searchMax $ submat a) ++ (searchMax $ submat c))
                                       ((searchMax $ submat b) ++ (searchMax $ submat d))
        submat q = Mat (n - 1) q

searchMin :: (Eq a, Show a, Num a, Ord a) => Mat a -> [a]
searchMin m = csum (mat m)
    where
        n = nexp m
        csum (C c)       = take (2 ^ n) $ repeat (c * 2 ^ n)
        csum (Q a b c d) = zipWith (\x y -> if(x<y) then x else y) ((searchMin $ submat a) ++ (searchMin $ submat c))
                                       ((searchMin $ submat b) ++ (searchMin $ submat d))
        submat q = Mat (n - 1) q

{- searchMinMax :: (Eq a, Num a, Show a, Ord a) => Mat a -> [(a,a)]
searchMinMax -}