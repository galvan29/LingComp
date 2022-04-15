{- 12. 
   Si scriva una funzione diff2next che, dato un albero binario di ricerca, costruisce un albero 
   binario di ricerca (annotato) di coppie dove il primo elemento di ogni coppia è l’elemento dell’albero
   originale mentre il secondo elemento è Just(la differenza rispetto al valore successivo), secondo
   l’ordinamento dei valori contenuti, oppure Nothing per il nodo di valore massimo. A titolo di esempio,
   
   Node 4 Void (Node 7 (Node 5 Void Void) Void)
      restituisce la soluzione
   Node (4,Just 1) Void (Node (7,Nothing) (Node (5,Just 2) Void Void) Void). 
-}

data BST a = Void | Node { val :: a, left, right :: BST a } deriving (Eq, Ord, Read, Show)

main = do
   print (diff2next (Node 4 (Node 3 Void Void) (Node 5 Void Void)))
   print (diff2next (Node 10 (Node 5 Void Void) (Node 15 Void Void)))


diff2next :: (Integral a, Num a, Show a) => BST a -> BST (a,String)
diff2next Void = Void
diff2next (Node x l r) = work 0 (Node x l r)

work :: (Integral a, Num a, Show a) => a -> BST a -> BST (a,String)
work b Void = Void 
work b (Node x l r) = Node (x,if(ciao)/=0 then ("Just "++(show (ciao-x))) else check) (work x l) (work 0 r)
 where {
    ciao = scendi (diff r) r;
    check = if(b-x)>0 then "Just "++(show (b-x)) else ("Nothing")
 }

scendi :: (Integral a, Num a) => a -> BST a -> a
scendi v Void = v
scendi v (Node x l r) = scendi x l

diff :: (Integral a, Ord a, Num a) => BST a -> a
diff Void = 0
diff (Node x l r) = x