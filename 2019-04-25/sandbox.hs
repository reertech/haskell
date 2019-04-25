import System.Random

data Response = Guessed | NotGuessed | Win | BailOut deriving (Show, Eq)
data Command = Guess Char | Quit deriving (Show, Eq)
data State = InitialState String | Game String String | Won String deriving (Show, Eq)

getCommand :: IO Command
getCommand = do
  s <- getLine
  return $ if s == "exit" || null s
              then Quit
              else Guess $ head s

showGuessed :: String -> String -> Char -> String
showGuessed word guessing ch =
  let f correct underscore = if correct == ch then correct else underscore
   in zipWith f word guessing

handleCommand :: State -> Command -> (State, Response)
handleCommand state@(Game word guessing) (Guess ch) =
  if (elem ch word && notElem ch guessing)
     then let newGuessing = showGuessed word guessing ch
           in if (elem '_' newGuessing)
                 then (Game word newGuessing, Guessed)
                 else (Won newGuessing, Win)
     else (state, NotGuessed)

handleCommand (InitialState word) command =
  handleCommand (Game word newGuessing) command
    where newGuessing = map (const '_') word

handleCommand state@(Won _) _ = (state, BailOut)

handleCommand state Quit = (state, BailOut)

handleResponse :: State -> Response -> IO ()
handleResponse (Game _ guessing) Guessed =
  putStrLn $ "You have guessed a letter. Here's what it looks like: " ++ guessing

handleResponse (Game _ guessing) NotGuessed =
  putStrLn $ "Sorry, wrong letter. Here's what it still looks like: " ++ guessing

handleResponse (Won guessing) Win =
  putStrLn $ "Pobedil, stervec! The word is: " ++ guessing

handleResponse _ BailOut =
  putStrLn "Do novyh vstrech"

handleResponse _ _ = return ()

fieldLoop :: State -> IO ()
fieldLoop state = do
  command <- getCommand
  let (newState, response) = handleCommand state command
  handleResponse newState response
  if response == BailOut || response == Win
     then return ()
     else fieldLoop newState

mkNewGame :: IO State
mkNewGame = do
  dictContents <- readFile "/usr/share/dict/words"
  let ws = lines dictContents
  randomElementNo <- randomRIO (0, length ws - 1)
  let w = ws !! randomElementNo
  return $ InitialState w

main :: IO ()
main = do
  initialState <- mkNewGame
  fieldLoop initialState

{-
   ДЗ:

     1. Доделать (сделать) телефонную книгу (прошлое ДЗ).
     2. Протестировать QuickCheck'ом инварианты одного из приложений (либо этого, либо телефонной книги).
-}
