{- 12. 
   Si scriva una funzione diff2next che, dato un albero binario di ricerca, costruisce un albero 
   binario di ricerca (annotato) di coppie dove il primo elemento di ogni coppia è l’elemento dell’albero
   originale mentre il secondo elemento è Just(la differenza rispetto al valore successivo), secondo
   l’ordinamento dei valori contenuti, oppure Nothing per il nodo di valore massimo. A titolo di esempio,
   
   Node 4 Void (Node 7 (Node 5 Void Void) Void)
      restituisce la soluzione
   Node (4,Just 1) Void (Node (7,Nothing) (Node (5,Just 2) Void Void) Void). 
-}