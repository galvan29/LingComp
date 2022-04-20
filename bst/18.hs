{-  18. Si scriva un predicato isBST, usando opportunamente fold, che dato un albero verifica se i valori
    in esso contenuti soddisfano la proprietà strutturale dei Binary Search Trees. -}

data BST a = Void | Node {val :: a, left, right :: BST a} deriving (Eq, Ord, Read, Show)

fold :: (Ord a) => (a -> b -> b -> b) -> b -> BST a -> Bool
fold _ z Void = z
fold f z (Node x l r) = f x (fold f z l) (fold f z r)

main = do
   print (isBST (Node 10 (Node 5 (Node 4 Void Void) (Node 6 Void (Node 7 Void Void))) (Node 15 Void Void)))
   print (isBST (Node 10 (Node 5 (Node 66 Void Void) (Node 6 Void (Node 7 Void Void))) (Node 15 Void Void)))

isBST :: (Ord a, Num a, Integral a, Eq a, Show a) => BST a -> Bool
isBST Void = True
isBST s = if(fold (\x y h -> if (x>y && x<h) then x else False) 0 s) then True else False
