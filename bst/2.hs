data BST a = Void | Node { val :: a, left, right :: BST a } deriving (Eq, Ord, Read, Show)

main = do
   print (sumBST (Node 1 (Node 2 (Node 1 Void Void) Void) Void))

sumBST :: (Integral a) => (BST a) -> a
sumBST Void = 0
sumBST (Node x l r) = (if (odd  x) then x else 0) + sumBST l + sumBST r 
