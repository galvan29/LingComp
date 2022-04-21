data BST a = Void | Node {val :: a, left, right :: BST a} deriving (Eq, Ord, Read, Show)

fold :: (Ord a) => (a -> b -> b -> b) -> b -> BST a -> Bool
fold _ z Void = z
fold f z (Node x l r) = f x (fold f z l) (fold f z r)

isBST :: (Ord a, Num a, Integral a, Eq a, Show a) => 