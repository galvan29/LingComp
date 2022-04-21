{- 19. Si scriva un predicato isAVL che dato un albero secondo la seguente definizione di tipo 
data (Ord a) => ABST a = Void | Node Bal a (ABST a) (ABST a) deriving (Eq, Ord, Read, Show) data Bal = Left | Bal | Right deriving (Eq, Ord, Read, Show) determina se è ben formato, cioè se • la differenza fra le profondità dei sottoalberi destro e sinistro di un qualunque nodo è al massimo 1; • le etichette Bal dei nodi sono consistenti con lo (s)bilanciamento. -}

data ABST a = Void | Node Bal a (ABST a) (ABST a) deriving (Eq, Ord, Read, Show) 
data Bal = Left | Bal | Right deriving (Eq, Ord, Read, Show)

isBST :: (Ord a, Num a, Integral a, Eq a, Show a) => 