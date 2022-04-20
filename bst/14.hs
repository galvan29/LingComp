{- 14. Si scriva una funzione treeheight per calcolare l’altezza di un albero usando opportunamente fold. -}

data BST a = Void | Node {val :: a, left, right :: BST a} deriving (Eq, Ord, Read, Show)

fold :: (Ord a) = > (a -> b -> b -> b) -> b -> BST a -> b
fold _ z Void = z
fold f z (Node x l r) = f x (fold f z l) (fold f z r)



main = do
   print (ciao [[1,1,1]])

treeheight :: (Ord a, Num a, Integer a) => BST a -> a
treeheight (Node x l r) = if (foldl (\z y->if z==y then 0 else 1) 0 (tail x))==0 then True else False

