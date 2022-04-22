data BST a = Void | Node {val :: a, left, right :: BST a} deriving (Eq, Ord, Read, Show)

fold :: (Ord a) => (a -> b -> b -> b) -> b -> BST a -> b
fold _ z Void = z
fold f z (Node x l r) = f x (fold f z l) (fold f z r)

main = do
   print (bst2List (Node 10 (Node 5 (Node 4 Void Void) (Node 6 Void (Node 7 Void Void))) (Node 15 Void Void)))
   print (bst2List (Node 10 (Node 5 (Node 66 Void Void) (Node 6 Void (Node 7 Void Void))) (Node 15 Void Void)))

bst2List :: (Ord a, Num a, Eq a, Show a) => BST a -> [a]
bst2List Void = []
bst2List tree = (fold (\x y z -> y ++ [x] ++ z) [] tree)