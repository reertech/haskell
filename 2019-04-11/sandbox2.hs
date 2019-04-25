data Developer = Developer { devName :: String
                           , devSurname :: String
                           , devAge :: Int
                           } deriving (Show)

data Commit = Commit { cmtHash :: String
                     , cmtDeveloper :: Developer
                     } deriving (Show)

sampleCommit :: Commit
sampleCommit = Commit "50c8a58379f44aa18f0c2e533d780ecb" $
                 Developer { devName = "Kolenqa", devSurname = "Sverchkov", devAge = 15 }

changeDevName :: String -> Commit -> Commit
changeDevName name commit =
  commit { cmtDeveloper = (cmtDeveloper commit) { devName = name } }

modifyDeveloper :: (Developer -> Developer) -> Commit -> Commit
modifyDeveloper f commit = commit { cmtDeveloper = f (cmtDeveloper commit) }

modifyName :: (String -> String) -> Developer -> Developer
modifyName f dev = dev { devName = f (devName dev) }

setName :: String -> (String -> String)
setName = const

changeDevName' :: String -> Commit -> Commit
changeDevName' newName commit =
  let someG = setName newName
      someF = modifyName someG
   in modifyDeveloper someF commit

changeDevName'' :: String -> Commit -> Commit
changeDevName'' newName commit =
  (modifyDeveloper . modifyName . setName $ newName) commit

changeDevName''' :: String -> Commit -> Commit
changeDevName''' = modifyDeveloper . modifyName . const

{-#

  ДЗ: написать телефонную книгу (без хранения данных в файле, только в памяти)

  1. Добавление записей.
  2. Удаление записей.
  3. Просмотр всех записей.
  4. Поиск по имеми (по вхождению, case insensitive).

  Всё это в текстовом меню.

  data Entry - запись
  data Phonebook = Phonebook [Entry]

  Подсказки: 1) удобно использовать линзы (наши);
             2) чтобы понять рекурсию надо понять рекурсию (хранение стейта).

  #-}
