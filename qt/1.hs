data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)

main = do
   print (buildNSimplify (C 1) (C 2) (C 3) (C 4))

buildNSimplify :: (Eq a, Show a) => QT a -> QT a -> QT a -> QT a -> QT a
buildNSimplify a b c d = (Q a b c d) 
