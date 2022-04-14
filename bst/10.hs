{- 10. Si scriva un predicato (funzione a valori booleani) almostBalanced per determinare se un albero
binario ha la seguente proprietà: per ogni nodo le altezze dei figli destro e sinistro differiscono al
massimo di 1. -}

data BST a = Void | Node { val :: a, left, right :: BST a } deriving (Eq, Ord, Read, Show)

main = do
   print (almostBalanced (Node 10 (Node 5 Void Void) (Node 15 Void Void)))

almostBalanced :: (Num a, Ord a) => BST a -> Bool
almostBalanced Void = True
almostBalanced (Node x l r) = if ((c-d) == -1 || (c-d) == 1 || (c-d) == 0) then (a && b) else False
  where {
      a = almostBalanced l;
      b = almostBalanced r;
      c = conta 0 l;
      d = conta 0 r;
  }

conta :: (Ord a, Num a) => a -> BST a -> a
conta v Void = v
conta v (Node x Void Void) = v+1
conta v (Node x Void r) = conta (v+1) r
conta v (Node x l Void) = conta (v+1) l
conta v (Node x l r) = if(conta (v+1) l > conta (v+1) r) then conta (v+1) l else conta (v+1) r