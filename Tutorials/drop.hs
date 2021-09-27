-- takes time proportional to the shortest of x and length xs
drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (x:xs) = drop n-1 xs
