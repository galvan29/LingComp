{- determina se è ben formato, cioè se
• ogni nodo contiene un valore non minore dei valori del suo sottoalbero sinistro e minore dei
valori del sottoalbero destro;
• tutti i cammini dalla radice a una foglia hanno lo stesso numero di nodi Black;
• i nodi Red devono avere genitore Black;
• la radice è Black. -}

data RBT a = Void | Node a Color (RBT a) (RBT a) deriving (Eq, Ord, Read, Show)
data Color = Red | Black deriving (Eq, Ord, Read, Show)

main = do
   print (isRBT (Node 10 Black (Node 5 Black Void Void) (Node 15 Black Void Void)))
   print (isRBT (Node 10 Black (Node 6 Black (Node 3 Red Void Void) (Node 8 Red (Node 7 Black Void Void) (Node 9 Black Void Void))) (Node 15 Black (Node 12 Black Void Void) (Node 17 Black Void Void))))
   print (isRBT (Node 6 Black (Node 3 Black Void Void) (Node 8 Red (Node 7 Black Void Void) (Node 9 Black Void Void))))

isRBT :: (Eq a, Ord a, Num a) => RBT a -> Bool
isRBT (Node x color l r) = rbheight (Node x color l r)


rbheight :: (Eq a, Ord a, Num a) => RBT a -> Bool
rbheight Void = True
rbheight (Node x color l r) = if((conta Black 0 l)==(conta Black 0 r)) then ((rbheight l) && (rbheight r)) else False

conta :: (Ord a, Num a) => Color -> a -> RBT a -> a
conta col v Void = (v+1)
conta col v (Node x color Void Void) = if(color==Black) then (v+1) else (v)
conta col v (Node x color Void r) = if(color==Black) then (conta col (v+1) r)+0 else 0+(conta col v r)
conta col v (Node x color l Void) = if(color==Black) then (conta col (v+1) l)+0 else 0+(conta col v l)
conta col v (Node x color l r) = if(color==Black) then ((conta col (v+1) l)+tr-tr) else ((conta col v l)+hh-hh)
  where {
      tr = (conta col (v+1) r);
      hh = (conta col v r);
  }