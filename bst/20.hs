{- determina se è ben formato, cioè se
• ogni nodo contiene un valore non minore dei valori del suo sottoalbero sinistro e minore dei
valori del sottoalbero destro;
• tutti i cammini dalla radice a una foglia hanno lo stesso numero di nodi Black;
• i nodi Red devono avere genitore Black;
• la radice è Black. -}

data RBT a = Void | Node a Color (RBT a) (RBT a) deriving (Eq, Ord, Read, Show)
data Color = Red | Black deriving (Eq, Ord, Read, Show)

main = do
   print (RBT (Node 10 Black (Node 5 Black (Node 5 Black Void Void) (Node 5 Black Void Void)) (Node 15 Black (Node 5 Black Void Void) (Node 5 Black Void Void))))

isRBT :: (Eq a, Ord a, Num a) => RBT a -> a
isRBT (Node x color l r) = rbheight (Node x color l r)


rbheight :: (Eq a, Ord a, Num a) => RBT a -> a
rbheight Void = 1
rbheight (Node x color l r) = 0 + if((rbheight l)==(rbheight r)) then (if(color==Black) then 1 else 0) else 0

conta :: (Ord a, Num a) => a -> RBT a -> a
conta v Void = v
conta v (Node x color Void Void) = if(color==Black) then v+1 else v
conta v (Node x Void r) = if(color==Black) then (conta (v+1) r) else (conta v r)
conta v (Node x l Void) = if(color==Black) then (conta (v+1) l) else (conta v l)
conta v (Node x l r) = if(conta (v+1) l == conta (v+1) r) then v
