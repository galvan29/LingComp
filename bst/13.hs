{- 13. Si scriva una funzione che dato un BST ne restituisce la lista degli elementi ottenuti visitando
    l’albero a livelli.
    Si consideri d’ora in poi la seguente generalizzazione a BST della funzione foldr su liste:
    fold :: ( Ord a ) = > ( a -> b -> b -> b ) -> b -> BST a -> b
    fold _ z Void = z
    fold f z ( Node x l r ) = f x ( fold f z l ) ( fold f z r )
    Ci si assicuri di scrivere funzioni lineari (non ha senso scrivere soluzioni che usino “forzosamente” una
    fold).
 -}

data BST a = Void | Node { val :: a, left, right :: BST a } deriving (Eq, Ord, Read, Show)
fold :: (Ord a) => (a -> b -> b -> b) -> b -> BST a -> b
fold _ z Void = z
fold f z (Node x l r) = f x (fold f z l) (fold f z r)



