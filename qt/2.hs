{- 2. Si scriva una funzione simplify che dato un termine di tipo QT genera il QuadTree corrispondente. -}

data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)

main = do
   print (simplify (Q (Q (C 1) (C 2) (C 3) (C 4)) (C 2) (C 3) (C 4)))

simplify :: (Eq a, Show a) => QT a -> QT a
simplify (Q f b c d) = (Q f b c d) 
