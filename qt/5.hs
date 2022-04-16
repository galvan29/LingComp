{- 5. Si scriva una funzione limitAll che dato un colore c e una lista di QuadTrees costruisca la lista
   dei QuadTrees che codificano le immagini i cui pixels sono limitati al colore c (pixel originale se il
   colore è < c, c altrimenti).
-}

data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)

main = do
   let f = (Q (Q (C 1) (C 2) (C 3) (C 4)) (C 2) (C 3) (C 4))
   print (limitAll (C 2) [f,f])

limitAll :: (Integral a, Eq a, Show a, Ord a, Num a) => QT a -> [QT a] -> [QT a]
limitAll c (x:[]) = [check c x]
limitAll c (x:xs) = [check c x] ++ limitAll c xs           

check :: (Integral a, Eq a, Show a, Ord a, Num a) => QT a -> QT a -> QT a
check (C c) (C k) = if(k<c) then (C c) else (C k)
check (C c) (Q f g h i) = (Q cf cg ch ci)
 where {
     cf = check (C c) f;
     cg = check (C c) g;
     ch = check (C c) h;
     ci = check (C c) i;
 }

