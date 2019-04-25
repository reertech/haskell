class MyShow a where
  myShow :: a -> String
  typeName :: a -> String

instance MyShow Bool where
  myShow False = "Nein"
  myShow True = "Ja"
  typeName _ = "Bool"

instance MyShow Int where
  myShow i = "Das Kapitan seight " ++ (show i)
  typeName _ = "Int"

data Content = MkInt Int | MkBool Bool deriving (Show, Read)

data Zolupa = Short | Long Content deriving (Show, Read)

instance MyShow Content where
  myShow (MkInt i) = myShow i
  myShow (MkBool b) = myShow b
  typeName _ = "Content"

superShow :: [Content] -> String
superShow as = "SUPER: " ++ concat (map myShow as)

{-
   ДЗ: реализовать тип, повторяющий поведение Ruby Boolean. Нужен сам тип и управляющая конструкция.

   Например,

   myIf "string" 1 2 == 1
   myIf 0 1 2 == 1
   myIf Nil 1 2 == 2

   main =
     myIf "string" (putStrLn "True") (putStrLn "False")
-}

class Mappable f where
  myMap :: (a -> b) -> f a -> f b

instance Mappable Maybe where
  myMap f (Just v) = Just $ f v
  myMap _ Nothing = Nothing

instance Mappable [] where
  myMap f arr = map f arr

class Eq a where
    (==), (/=) :: a -> a -> Bool
    x /= y = not (x == y)
    x == y = not (x /= y)

class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y = if x == y then EQ
                  else if x <= y then LT
                  else GT

    x <  y = case compare x y of { LT -> True;  _ -> False }
    x <= y = case compare x y of { GT -> False; _ -> True }
    x >  y = case compare x y of { GT -> True;  _ -> False }
    x >= y = case compare x y of { LT -> False; _ -> True }

    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
