import Debug.Trace
import Data.Maybe

{- where -}
test1 :: Int -> String -> String
test1 len name =
  concat [name, " dick length is ", lengthString, "cm"]
    where anotherName = show
          lengthString = anotherName len

{- let -}
test2 :: Int -> String -> String
test2 len name =
  let anotherName = show
      lengthString = anotherName len
   in let unit = "cm"
   in concat [name, " dick length is ", lengthString, unit]

{- case -}
test3 :: Int -> String -> [String]
test3 len name =
  case name of
    "Rustam" -> ["No, I'm not talking about Rustam's cock"]
    "" -> []
    _ -> [test1 len name]

handleTest3Result :: [String] -> IO ()
handleTest3Result test3Result =
  case test3Result of
    [] -> putStrLn "You need to specify a name"
    [msg] -> putStrLn msg
    _ -> error "Nothing nahooy blead!"

{- String синоним [Char] -}

{- data Maybe a = Just a | Nothing -}

division' :: Int -> Int -> Maybe Int
division' a b =
  if b == 0
     then Nothing
     else Just (a `div` b)

multiply :: Int -> Maybe Int
multiply x = if x > 100
                then Nothing
                else Just $ x * 2

division :: Int -> Int -> IO ()
division a b =
  let result = (fmap (+ 1) $ division' a b) >>= multiply
   in case result of
        Nothing -> putStrLn "Sluchilsya pizdec"
        Just value -> putStrLn $ "OK, the result is " ++ show value

shtany :: IO ()
shtany = getLine >>= putStrLn

shtany' :: IO ()
shtany' = do
  v <- getLine
  putStrLn v

shtany'' :: IO ()
shtany'' = getLine >>= (\v -> putStrLn v)

zolupa :: Int -> Int -> Maybe Int
zolupa a b = do
  divres <- fmap (+ 1) $ division' a b
  multiply divres

{-
   ДЗ: сделать решатель квадратных уравнений вида:

    a * x^2 + b * x + c = 0

   Выделить отдельно функцию вычисления дискриминанта.

   Сделать два варианта: с использованием do-нотации и с использованием стрелки Клейсли.
-}


