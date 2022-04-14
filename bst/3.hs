data BST a = Void | Node { val :: a, left, right :: BST a } deriving (Eq, Ord, Read, Show)

main = do
   print (samesums [(Node 1 (Node 2 (Node 1 Void Void) Void) Void), (Node 1 (Node 2 (Node 1 Void Void) Void) Void), (Node 1 (Node 2 (Node 1 Void Void) Void) Void)])

samesums :: (Integral a) => [(BST a)] -> Bool
samesums (x:[]) = True
samesums (x:y:xs) = if(sumBST x == sumBST y) then samesums (y:xs) else False

sumBST :: (Integral a) => (BST a) -> a
sumBST Void = 0
sumBST (Node x l r) = (if (odd  x) then x else 0) + sumBST l + sumBST r 