data BST a = Void | Node { val :: a, left, right :: BST a } deriving (Eq, Ord, Read, Show)

main = do
   print (bst2List (\x -> x>3) (Node 10 (Node 5 (Node 2 Void (Node 4 Void Void)) Void) Void))

bst2List :: (Ord a) => (a -> Bool) -> BST a -> [a]
bst2List f (Node x Void r) = if(f(x)) then [x] else [] ++ bst2List f r
bst2List f (Node x l Void) = bst2List f l ++ if(f(x)) then [x] else []
bst2List f Void = []