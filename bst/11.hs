data WBST a = Void | Node { val :: a, w :: Int, left, right :: WBST a } deriving (Eq, Ord, Read, Show)

main = do
   print (insert (Node 10 2 (Node 5 1 Void Void) (Node 15 1 Void Void)) 16 0)

insert :: (Num a, Ord a) => WBST a -> a -> Int -> WBST a
insert Void v c = Node v c Void Void
insert (Node x w l r) v w1
    | x == v = (Node x w l r)
    | x < v  = Node x w l (insert r v (w+1))
    | x > v  = Node x w (insert l v (w+1)) r