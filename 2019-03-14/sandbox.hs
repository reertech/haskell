quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (p:xs) =
  (quicksort lesser) ++ [p] ++ (quicksort greater)
    where lesser = filter (< p) xs
          greater = filter (>= p) xs

supersum 0 = 0
supersum n = supersum (n-1) + n


seive :: [Int]

{-

Сделать список простых чисел от 1 до 100 (например, при помощи решета Эротохера)

-}
