{- 3. Si scriva una funzione map che data una funzione f e un QuadTree q determina il QuadTree che
codifica l’immagine risultante dall’applicazione di f a tutti i pixel dell’immagine codificata da q. -}

data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)

main = do
   print (map2 (\x -> x>2) (Q (Q (C 1) (C 2) (C 3) (C 4)) (C 2) (C 3) (C 4)))

map2 :: (Num a, Eq a, Show a) => (a -> Bool) -> QT a -> QT a
map2 fu (C s) = if(fu(s)) then C s else C (-1)
map2 fu (Q g h i l) = Q (map2 fu g) (map2 fu h) (map2 fu i) (map2 fu l)