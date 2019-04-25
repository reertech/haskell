pidorTest' :: Int -> Bool
pidorTest' d = d > 1

pidorTest'' :: Int -> String
pidorTest'' d
  | d > 30 = "TY denis :("
  | d > 20 = "TY GNOINY PIDR"
  | d > 10 = "TY tochno PIDR"
  | d > 1 = "TY PIDR"
  | otherwise = "Ok, you're not pidor"

pidorTest :: String -> String
pidorTest s = pidorTest'' (read s)

main = do
  putStrLn "Let's test if you are a pidor..."
  putStrLn "Enter diameter:"
  input <- getLine
  putStrLn (pidorTest input)
