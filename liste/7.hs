main = do
   print (shiftToZero [5,4,2,6])
   
shiftToZero :: [Integer] -> [Integer]
shiftToZero (x:xs) = [5]

{- dovrei trattare tutto richiamando una tupla con il valore, il minimo fino a quel momento e poi quando
 risalgo dovrei, tramite il where andare a vedere se il passato è minore di quello che ho in quel momento.
 Prendo il minore e lo sottraggo ritornando una lista con quel valore che ho ottenuto dalla sottrazione -}