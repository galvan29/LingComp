{- 14. Si scriva una funzione treeheight per calcolare l’altezza di un albero usando opportunamente fold. -}

main = do
   print (ciao [[1,0,0]])

ciao :: [[Integer]] -> Bool
ciao (x:xs) = if (foldl (\z y->if z==y then 0 else 1) 0 (tail x))==1 then True else False

