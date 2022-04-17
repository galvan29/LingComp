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