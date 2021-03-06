import Control.Monad

data Roots = Root Double | Roots Double Double deriving (Show)

countSqd :: Monad m => Double -> Double -> Double -> m Double
countSqd a b c =
  let d = b*b - 4*a*c
   in if d >= 0
         then return $ sqrt d
         else fail "Error"

solveWithSqD :: Double -> Double -> Double -> Double -> Roots
solveWithSqD a b _ 0.0 = Root $ -b / (2*a)
solveWithSqD a b c sqd =
  Roots x1 x2
    where f sign = -b + sqd/(2*a)*sign
          x1 = f (-1)
          x2 = f 1

solve :: Monad m => Double -> Double -> Double -> m Roots
solve a b c = solveWithSqD a b c <$> countSqd a b c

main = do
  a <- getLine
  b <- getLine
  c <- getLine
  result <- solve (read a) (read b) (read c)
  print result

putEither (Left err) = putStrLn $ "Oshibka: " ++ err
putEither (Right val) = print val

{-
   1. Сделать, чтобы всё не обсиралось при неправильном вводе (используя Maybe).
   2. Использовать readMaybe, как безопасную версию read.
   3. Скомпоновать всю программу в Maybe, чтобы ошибки на ранних стадиях приводили к корректному завершению.
   4. Задача на пятёрку: попытаться вывести сообщение об ошибке при помощи Either, без полиморфности.
-}
