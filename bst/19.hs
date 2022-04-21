{- 19. Si scriva un predicato isAVL che dato un albero secondo la seguente definizione di tipo 
data (Ord a) => ABST a = Void | Node Bal a (ABST a) (ABST a) deriving (Eq, Ord, Read, Show) data 
Bal = Left | Bal | Right deriving (Eq, Ord, Read, Show) determina se è ben formato, 
cioè se • la differenza fra le profondità dei sottoalberi destro e sinistro di un 
qualunque nodo è al massimo 1; • le etichette Bal dei nodi sono consistenti con lo (s)bilanciamento. -}

Funziona ma manca un caso base penso
Check anche etichette in realtà ma sono degli if


data ABST a = Void | Node Bal a (ABST a) (ABST a) deriving (Eq, Ord, Read, Show) 
data Bal = Left | Bal | Right deriving (Eq, Ord, Read, Show)

fold :: (Ord a) => (a -> b -> b -> b) -> b -> ABST a -> b
fold _ z Void = z
fold f z (Node bal x l r) = f x (fold f z l) (fold f z r)

main = do
  print (isAVL (Node Main.Left 10 (Node Bal 6 (Node Bal 3 (Node Bal 2 Void Void) (Node Bal 5 Void Void)) (Node Bal 7 Void Void)) (Node Bal 15 Void Void)))

isAVL :: (Ord a, Num a, Integral a, Eq a, Show a) => ABST a -> Bool
isAVL Void= True
isAVL (Node Bal a l r) = if(check) then (isAVL l) && (isAVL r) else False 
  where{
      check = if(minus==1) then True else if(minus==(-1)) then True else if(minus==0) then True else False;
      minus = (treeheight l)-(treeheight r);
  }

treeheight :: (Ord a, Num a, Integral a, Eq a, Show a) => ABST a -> a
treeheight s = fold (\x y h -> 1 + max y h) 0 s

