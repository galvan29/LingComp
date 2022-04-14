data BST a = Void | Node { val :: a, left, right :: BST a } deriving (Eq, Ord, Read, Show)

main = do
   print (bst2List (Node 10 (Node 5 (Node 2 Void (Node 4 Void Void)) Void) Void))

bst2List :: (Ord a) => BST a -> [a]
bst2List (Node x Void r) = x : bst2List r
bst2List (Node x l Void) = bst2List l ++ [x]
bst2List Void = []