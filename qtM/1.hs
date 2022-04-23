    {- Grazie ai Quad Trees introdotti nella sezione precedente si possono implementare certe operazioni
    matriciali, nel caso dei linguaggi funzionali puri ovviamente, in modo molto più efficiente.
    Si implementino matrici 2
    n × 2
    n utilizzando il seguente tipo di dato astratto (polimorfo)
    data ( Eq a , Num a , Show a ) = > Mat a = Mat {
    nexp :: Int ,
    mat :: QT a
    }
    deriving ( Eq , Show )
    dove nel campo mat non metteremo mai solo “termini di tipo QT” ma QuadTrees “propri”.
    1. Si scriva un predicato lowertriangular che determina se una matrice è triangolare inferiore.
    Attenti a cosa devono restituire
    lowertriangular $ Mat 0 (C 2) e lowertriangular $ Mat 1 (C 2). -}

data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   let z = (Q (C 1) (C 0) (C 0) (C 1))
   let ab = (Q (C 0) (C 0) (C 1) (C 0))
   print (lowertriangular (Mat 2 (Q z (C 2) ab z)))

lowertriangular Mat{nexp=n, mat=(C x)} = x==0 || n==0
lowertriangular (Mat n (Q (C x1) (C x2) x3 (C x4))) = if n>0 then ((lowertriangular Mat{nexp=(n-1), mat=x3})) else False
lowertriangular (Mat n (Q x1 x2 x3 x4)) = if n>0 then 
                ((lowertriangular (Mat (n-1) x1)) && (lowertriangular (Mat (n-1) x3)) && (lowertriangular (Mat (n-1) x4))) else False

