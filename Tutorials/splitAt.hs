-- Takes time proportional to n or length xs whichever is shorter,
--  and is twice as fast as using take or drop explicitly
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs | n <= 0 = ([], xs)
splitAt _ [] = ([], [])
splitAt n (x:xs)
  = let (xs1, xs2) splitAt n-1 xs
  in (x:xs1, xs2)