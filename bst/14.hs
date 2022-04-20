{- 14. Si scriva una funzione treeheight per calcolare l’altezza di un albero usando opportunamente fold. -}

data BST a = Void | Node {val :: a, left, right :: BST a} deriving (Eq, Ord, Read, Show)

fold :: (Ord a) => (a -> b -> b -> b) -> b -> BST a -> b
fold _ z Void = z
fold f z (Node x l r) = f x (fold f z l) (fold f z r)

main = do
   print (treeheight (Node 10 (Node 5 (Node 4 Void Void) (Node 6 Void (Node 7 Void Void))) (Node 15 Void Void)))

treeheight :: (Ord a, Num a, Integral a, Eq a, Show a) => BST a -> a
treeheight s = fold (\x y h -> 1 + max y h) 0 s
