data BST a = Void | Node { val :: a, left, right :: BST a } deriving (Eq, Ord, Read, Show)

main = do
   print (bstElem (Node 1 (Node 2 (Node 1 Void Void) Void) Void) 4)
   print (bstElem (Node 1 (Node 2 (Node 1 Void Void) Void) Void) 2)
   print (bstElem2 (Node 1 (Node 2 (Node 1 Void Void) Void) Void) 4)
   print (bstElem2 (Node 1 (Node 2 (Node 1 Void Void) Void) Void) 2)

bstElem :: (Ord a) => (BST a) -> a -> Bool
bstElem Void el = False
bstElem (Node x l r) el = if(x==el) then True else ((bstElem l el) || (bstElem r el))

bstElem2 :: (Ord a) => (BST a) -> a -> Bool
bstElem2 Void el = False
bstElem2 (Node x l r) el 
   | x == el = True 
   | x < el = bstElem2 l el
   | x > el = bstElem2 r el