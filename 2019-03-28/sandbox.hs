x :: Int
x = 2

y :: Int
y = 3

test :: Double
test =
  let [xd, yd] = map fromIntegral [x, y]
   in xd * yd

main = putStrLn $ show $ 2 * 0.5
