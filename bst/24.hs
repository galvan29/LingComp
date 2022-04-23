{-  24. Si scriva una funzione limitedVisit che dato un BST e due valori x, y costruisce la lista (ordinata)
    degli elementi dell’albero compresi nell’intervallo di valori da x a y.
-}

data BST a = Void | Node {val :: a, left, right :: BST a} deriving (Eq, Ord, Read, Show)

fold :: (Ord a) => (a -> b -> b -> b) -> b -> BST a -> b
fold _ z Void = z
fold f z (Node x l r) = f x (fold f z l) (fold f z r)

main = do
   print (bst2List 5 10 (Node 10 (Node 5 (Node 4 Void Void) (Node 6 Void (Node 7 Void Void))) (Node 15 Void Void)))

bst2List :: (Ord a, Num a, Eq a, Show a) => a -> a -> BST a -> [a]
bst2List f d Void = []
bst2List f d tree = (fold (\x y z -> y ++ (if(x>f && x<d) then [x] else []) ++ z) [] tree)