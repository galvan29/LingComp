{- 9. Si scriva una funzione annotate che costruisca un nuovo BST che in ogni nodo contenga, al posto
    del valore originale, una coppia composta dal medesimo valore e dall’altezza del nodo stesso (la
    lunghezza del massimo cammino, cioè 1 + max(height(sx), height(dx)). Si scelga di attribuire
    all’albero vuoto 0 o -1 a seconda delle preferenze.
 -}

data BST a = Void | Node {val :: a, left, right :: BST a} deriving (Eq, Ord, Read, Show)

fold :: (Ord a) => (a -> b -> b -> b) -> b -> BST a -> b
fold _ z Void = z
fold f z (Node x l r) = f x (fold f z l) (fold f z r)

main = do
   print (annotate (Node 10 (Node 5 (Node 4 Void Void) (Node 6 Void (Node 7 Void Void))) (Node 15 Void Void)))

treeheight :: (Ord a, Num a, Integral a, Eq a, Show a) => BST a -> a
treeheight s = fold (\x y h -> 1 + max y h) 0 s

annotate :: (Ord a, Num a, Integral a, Eq a, Show a) => BST a -> BST (a,a)
annotate Void = Void
annotate (Node x l r) = (Node (x,treeheight (Node x l r)) (annotate l) (annotate r))