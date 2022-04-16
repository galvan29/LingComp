{- 7. Si scriva una funzione Haskell difference che dato un colore c ed un QuadTree q determina la
   differenza fra il numero di pixel dell’immagine codificata da q che hanno un colore maggiore di c e
   quelli minori di c. Ad esempio
   let d = C 2; u = C 1; q = Q d u u u
   in difference 1 ( Q q ( C 0) (C 3) q )
   restituisce -4 (visto che il QuadTree codifica almeno 16 pixel). 
-}