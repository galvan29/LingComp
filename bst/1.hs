data BST a = Void | Node { val :: a, left, right :: BST a } deriving (Eq, Ord, Read, Show)

main = do
   print (sumBST (Node 10 (Node 3 (Node 2 Void Void) Void) Void))

sumBST :: (Num a) => (BST a) -> a
sumBST Void = 0
sumBST (Node x l r) = x + sumBST l + sumBST r 
