fib :: Int -> Int
fib n
  | n == 1 = 0
  | n == 2 = 1
  | otherwise = fib (n-1) + fib (n-2)

fib' :: Int -> Int
fib' 1 = 0
fib' 2 = 1
fib' n = fib' (n-1) + fib' (n-2)

test 1 = 1
test 2 = 4

firstElem (x:xs) = x

restElems (x:xs) = xs

f :: [Int] -> String
f xs
  | xs == [] = error "Day spisok blead"
  | otherwise = if (head xs > 10)
                  then "Big deek"
                  else "Little dick"
