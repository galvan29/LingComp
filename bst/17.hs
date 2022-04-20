{- 17. Si scriva una funzione maxDiameter che data una lista l di BST determina il massimo dei diametri dei
   BST di l. Il diametro di un BST è la lunghezza del massimo cammino fra due nodi,
   indipendentemente dall’orientamento degli archi.
-}

data BST a = Void | Node {val :: a, left, right :: BST a} deriving (Eq, Ord, Read, Show)

fold :: (Ord a) => (a -> b -> b -> b) -> b -> BST a -> b
fold _ z Void = z
fold f z (Node x l r) = f x (fold f z l) (fold f z r)

main = do
   let uno = (Node 10 (Node 5 (Node 4 Void Void) (Node 6 Void (Node 7 Void Void))) (Node 15 Void Void))
   let due = (Node 10 (Node 5 Void Void) (Node 15 Void Void))
   print (maxDiameter [uno,due])

maxDiameter :: (Ord a, Num a, Integral a, Eq a, Show a) => [BST a] -> a
maxDiameter list = max2 (checkList list)

checkList :: (Ord a, Num a, Integral a, Eq a, Show a) => [BST a] -> [a]
checkList [] = []
checkList (x:xs) = (treeheight x) : checkList xs

max2 :: (Ord a, Num a, Integral a, Eq a, Show a) => [a] -> a
max2 (x:[]) = x
max2 (x:y:xs) = if(x>y) then max2 (x:xs) else max2 (y:xs)

treeheight :: (Ord a, Num a, Integral a, Eq a, Show a) => BST a -> a
treeheight s = fold (\x y h -> 1 + max y h) 0 s

