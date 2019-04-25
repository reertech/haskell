import Test.QuickCheck

{--------
   Pure
---------}
output' :: String -> String
output' name =
  if name == "Denis"
     then "Sosi Khui"
     else "Welcome to Haskell, " ++ name ++ "!"

{----------
   Tests
-----------}
propOutput1 :: String -> Property
propOutput1 a = a /= "Denis" && length a < 5 ==> output' a == ("Welcome to Haskell, " ++ a ++ "!")

propOutput2 :: Bool
propOutput2 = output' "Denis" == "Sosi Khui"

runTests :: IO ()
runTests = do
  quickCheck propOutput1
  quickCheck propOutput2


{------------------
   Implementation
-------------------}
handle :: (String -> String) -> IO ()
handle f = do
  putStrLn "Hello. What is your name?"
  putStrLn "Please, enter it below:"
  name <- getLine
  putStrLn (f name)

main :: IO ()
main = handle output'
