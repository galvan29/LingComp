data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)

main = do
   {-print (buildNSimplify (C 1) (C 2) (C 3) (C 4))-}
   print (fu 4 (Q (C 1) (C 2) (C 3) (C 4)))

buildNSimplify :: (Eq a, Show a) => QT a -> QT a -> QT a -> QT a -> QT a
buildNSimplify a b c d = (Q a b c d) 


fu :: (Eq a, Show a, Num a, Ord a) => a -> QT a -> QT a
fu n (C x) = if(n>0) then (fu (n-1) (Q (C x) (C x) (C x) (C x))) else (C 1)
fu n (Q a b c d) = if(n>0) then (Q (fu (n-1) a) (fu (n-1) b) c d) else (Q (C 1) (C 1) c d)
