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
  