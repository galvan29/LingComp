{- 10. Si scriva un predicato (funzione a valori booleani) almostBalanced per determinare se un albero
    binario ha la seguente proprietà: per ogni nodo le altezze dei figli destro e sinistro differiscono al
    massimo di 1.
-}

data BST a = Void | Node {val :: a, left, right :: BST a} deriving (Eq, Ord, Read, Show)

fold :: (Ord a) => (a -> b -> b -> b) -> b -> BST a -> b
fold _ z Void = z
fold f z (Node x l r) = f x (fold f z l) (fold f z r)

main = do
   print (almostBalanced (Node 10 (Node 5 (Node 4 Void Void) (Node 6 Void (Node 7 Void Void))) (Node 15 Void Void)))
   print (almostBalanced (Node 10 (Node 5 Void Void) (Node 15 Void Void)))

treeheight :: (Ord a, Num a, Integral a, Eq a, Show a) => BST a -> a
treeheight s = fold (\x y h -> 1 + max y h) 0 s

almostBalanced :: (Ord a, Num a, Integral a, Eq a, Show a) => BST a -> Bool
almostBalanced Void = True
almostBalanced (Node x l r) = if (val==0 || val==1) then ((almostBalanced l) && (almostBalanced r)) else False
 where{
    val = abs((treeheight l)-(treeheight r));
 }
