{- determina se è ben formato, cioè se
• ogni nodo contiene un valore non minore dei valori del suo sottoalbero sinistro e minore dei
valori del sottoalbero destro;
• tutti i cammini dalla radice a una foglia hanno lo stesso numero di nodi Black;
• i nodi Red devono avere genitore Black;
• la radice è Black. -}

data RBT a = Void | Node a Color (RBT a) (RBT a) deriving (Eq, Ord, Read, Show)
data Color = Red | Black deriving (Eq, Ord, Read, Show)

isRBT :: (Eq a, Ord a, Num a) => RBT a -> Bool
isRBT (Node x color l r) = 


