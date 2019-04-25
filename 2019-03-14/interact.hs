import System.IO

response :: String -> (String, String)
response inp =
  (rest, "Hello, " ++ name)
  where (name, rest) = span (/= '\n') inp

response2 :: String -> (String, String)
response2 inp =
  (rest, "Hello again, " ++ name)
  where (name, rest) = span (/= '\n') inp

test :: String -> String
test s =
  unlines $ "Hello, what is your name:" : r1 : "Again?" : r2 : []
    where (r1s, r1) = response s
          (_, r2) = response2 r1s

main = do
  hSetEcho stdin False
  interact test
