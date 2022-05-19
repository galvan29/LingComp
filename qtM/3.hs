data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)  
data Mat a = Mat {nexp :: Int, mat :: QT a} deriving ( Eq , Show )

main = do
   let z = (Q (C 1) (C 0) (C 0) (C 1))
   print (diagonal (Mat 2 (Q z (C 0) (C 0) z)))
   print ((Just [1]) ++ (Just [2]))

diagonal (Mat n (Q x1 x2 x3 x4)) = if(diagonal3 (Mat n (Q x1 x2 x3 x4))) && (diagonal2 (Mat n (Q x1 x2 x3 x4))) then True else False

diagonal3 Mat{nexp=n, mat=(C x)} = x/=0 || n==0
diagonal3 (Mat n (Q x1 (C x2) (C x3) x4)) = if n>0 then ((diagonal3 Mat{nexp=(n-1), mat=x1}) && (diagonal3 Mat{nexp=(n-1), mat=x4})) else False
diagonal3 (Mat n (Q x1 x2 x3 x4)) = if n>0 then 
                ((diagonal3 (Mat (n-1) x1)) && (diagonal3 (Mat (n-1) x4))) else False

diagonal2 Mat{nexp=n, mat=(C x)} = x==0 || n==0
diagonal2 (Mat n (Q (C x1) x2 x3 (C x4))) = if n>0 then ((diagonal2 Mat{nexp=(n-1), mat=x2}) && (diagonal2 Mat{nexp=(n-1), mat=x3})) else False
diagonal2 (Mat n (Q x1 x2 x3 x4)) = if n>0 then 
                ((diagonal2 (Mat (n-1) x2)) && (diagonal2 (Mat (n-1) x3))) else False

