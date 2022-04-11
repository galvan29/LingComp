fat 0 = 1
fat x = x*fat (x-1)

{- Combinatoria -}
combinatoria :: Integer -> Integer -> Integer
combinatoria n k = a `div` (b*c)
  where {
     a = fat n;
     b = fat k;
     c = fat (n-k);
  }

comb :: Integer -> [Integer]
comb n =  work [1..n]
  where
    work [] = []
    work (x:xs) = combinatoria n x:work xs

comb2 :: Integer -> [Integer]
comb2 n = work2 [1..n]
  where
    work2 (x:xs) = combinatoria n x : map (combinatoria n) xs

comb3 :: Integer -> [Integer]
comb3 n = map (combinatoria n) [1..n]