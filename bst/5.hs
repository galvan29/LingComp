data BST a = Void | Node { val :: a, left, right :: BST a } deriving (Eq, Ord, Read, Show)

main = do
   print (insert (Node 10 (Node 5 (Node 2 Void Void) Void) Void) 7)

insert :: (Ord a) => BST a -> a -> BST a
insert Void v = Node v Void Void
insert (Node x l r) v 
   | x == v = Node x l r
   | x < v = Node x l (insert r v)
   | x > v = Node x (insert l v) r
