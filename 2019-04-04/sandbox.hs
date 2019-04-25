import Control.Monad
import Text.Read

data Roots = Root Double | Roots Double Double deriving (Show)

countSqd :: Double -> Double -> Double -> Either String Double
countSqd a b c =
  let d = b*b - 4*a*c
   in if d >= 0
         then Right $ sqrt d
         else Left "Unable to calculate discriminant"

solveWithSqD :: Double -> Double -> Double -> Double -> Roots
solveWithSqD a b _ 0.0 = Root $ -b / (2*a)
solveWithSqD a b c sqd =
  Roots x1 x2
    where f sign = -b + sqd/(2*a)*sign
          x1 = f (-1)
          x2 = f 1

solve :: Double -> Double -> Double -> Either String Roots
solve a b c = solveWithSqD a b c <$> countSqd a b c

maybeSolve :: Either String Double -> Either String Double -> Either String Double -> Either String Roots
maybeSolve ma mb mc = do
  a <- ma
  b <- mb
  c <- mc
  solve a b c

ensureEntered :: String -> String -> IO ()
ensureEntered val comment =
  when (val == "") $ error $ "No " ++ comment ++ " entered"

myReadEither :: String -> String -> Either String Double
myReadEither comment val =
  case readEither val of
    Left err -> Left comment
    Right parsedValue -> Right parsedValue

main = do
  a <- getLine
  ensureEntered a "a"
  b <- getLine
  ensureEntered b "b"
  c <- getLine
  ensureEntered c "c"
  let result = maybeSolve (myReadEither "wrong a" a) (myReadEither "wrong b" b) (myReadEither "wrong c" c)
  print result

putEither (Left err) = putStrLn $ "Oshibka: " ++ err
putEither (Right val) = print val

eitherTest :: Either String Int
eitherTest = do
  a <- Left "Pizdec"
  b <- Left "Esho pizdec"
  Right $ 123


{-
   1. Сделать, чтобы всё не обсиралось при неправильном вводе (используя Maybe).
   2. Использовать readMaybe, как безопасную версию read.
   3. Скомпоновать всю программу в Maybe, чтобы ошибки на ранних стадиях приводили к корректному завершению.
   4. Задача на пятёрку: попытаться вывести сообщение об ошибке при помощи Either, без полиморфности.
-}
