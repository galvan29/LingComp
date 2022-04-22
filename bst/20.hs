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
   print (checkRed Black (Node 10 Black (Node 5 Black Void Void) (Node 15 Black (Node 13 Red (Node 11 Black Void Void) (Node 14 Black Void Void)) (Node 17 Black Void Void))))

isRBT :: (Eq a, Ord a, Num a) => RBT a -> Bool
isRBT (Node x color l r) = rbheight (Node x color l r)


rbheight :: (Eq a, Ord a, Num a) => RBT a -> Bool
rbheight Void = True
rbheight (Node x color l r) = if((conta 0 l)==(conta 0 r)) then ((rbheight l) && (rbheight r)) else False

conta :: (Ord a, Num a) => a -> RBT a -> a
conta v Void = (v+1)
conta v (Node x color Void Void) = if(color==Black) then (v+1) else (v)
conta v (Node x color Void r) = if(color==Black) then (conta (v+1) r) else (conta v r)
conta v (Node x color l Void) = if(color==Black) then (conta (v+1) l) else (conta v l)
conta v (Node x color l r) = if(color==Black) then (conta (v+1) l) else ((conta v l))

checkRed :: (Eq a, Ord a, Num a) => Color -> RBT a -> Bool
checkRed col Void = True
checkRed col (Node x color Void Void) = if(color==Black) then True else if(color==Red && col==Black) then True else False
checkRed col (Node x color l r) = if(color==Black) then ye else if (color==Red && col==Black) then ye else False 
 where {
     ye = ((checkRed color l) && (checkRed color r));
 }