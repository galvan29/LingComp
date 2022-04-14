data BST a = Void | Node { val :: a, left, right :: BST a } deriving (Eq, Ord, Read, Show)

main = do
   print (annotate (Node 10 (Node 5 (Node 2 Void Void) Void) Void))
   print (conta 0 (Node 10 (Node 5 (Node 2 Void Void) Void) Void))

annotate :: (Num a, Ord a) => BST a -> BST (a,a)
annotate Void = Void
annotate (Node x l r) = Node (x, if (c > d) then c else d) a b
  where {
      a = annotate l;
      b = annotate r;
      c = conta 0 l;
      d = conta 0 r;
  }

conta :: (Ord a, Num a) => a -> BST a -> a
conta v Void = v
conta v (Node x Void Void) = v+1
conta v (Node x Void r) = conta (v+1) r
conta v (Node x l Void) = conta (v+1) l
conta v (Node x l r) = if(conta (v+1) l > conta (v+1) r) then conta (v+1) l else conta (v+1) r