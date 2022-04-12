{- Si scriva un predicato diagonal che dica se una matrice è diagonale -}
{- Una matrice diagonale è una matrice in cui sono presenti numeri solamente nella parte diagonale da sinistra-alto fino a destra-basso -}

main = do
    print (diagonal [[1,0,0],[0,5,0],[0,0,9]])
    print (diagonal [[1,0,0],[0,5,0],[0,1,9]])

diagonal :: [[Integer]] -> Bool
diagonal (x:xs) = if ((a==True) && (b==True)) then True else False
 where {
    a = lovertriangular (x:xs)
    b = 
 }
 

lovertriangular [] = True
lovertriangular (x:xs) = if (foldl (\z y->if z==y then 0 else 1) 0 (tail x))==0 then lovertriangular (map tail xs) else False