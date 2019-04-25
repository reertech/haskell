pidorTest' :: Int -> Bool
pidorTest' d = d > 1

pidorTest :: String -> String
pidorTest s =
  if pidorTest' (read s)
     then "TY PIDR"
     else "Ok, you're not pidor"

main = do
  putStrLn "Let's test if you are a pidor..."
  putStrLn "Enter diameter:"
  input <- getLine
  putStrLn (pidorTest input)
