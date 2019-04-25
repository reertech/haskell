{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Name = Name String deriving (Show)
data Developer = Junior Name | Senior Name deriving (Show)

mkJuniorDeveloper :: Name -> Developer
mkJuniorDeveloper n = Junior n

data Temp = Celsius Double | Kelvin Double deriving (Show)

heat :: Temp -> Temp -> Temp
heat = add

mkTempGeneric :: (Double -> Temp) -> Double -> Double -> Temp
mkTempGeneric f thresh t = if t >= thresh
                       then f t
                       else error "Invalid temperature"

mkCelsius :: Double -> Temp
mkCelsius = mkTempGeneric Celsius (-273.0)

mkKelvin :: Double -> Temp
mkKelvin = mkTempGeneric Kelvin 0.0

add :: Temp -> Temp -> Temp
add (Celsius a) (Celsius b) = mkCelsius $ a + b
add (Kelvin a) (Kelvin b) = mkKelvin $ a + b
add (Celsius a) (Kelvin b) = mkKelvin $ a + b


main = putStrLn "kuku, ebta"
