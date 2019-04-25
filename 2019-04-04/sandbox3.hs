matchCond :: (Char -> Boolean) -> String -> Maybe (String, Char)
matchCond f (x : xs) =
  if f x
     then Just $ (xs, x)
     else Nothing

matchDigit :: String -> Maybe (String, Char)
matchDigit = matchCond isDigit

matchNumber :: String -> Maybe (String, String)
matchNumber s =
