sieve :: Int -> [Int] -> [Int]
sieve n xs =
  if n < 100
     then if any (== 0) (map (n `mod`) xs)
             then sieve (n+1) xs
             else sieve (n+1) (n:xs)
     else xs

sieve' :: [Int]
sieve' = sieve 2 []
