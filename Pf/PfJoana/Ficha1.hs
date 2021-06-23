module Ficha1 where
import Data.Char 

---2
--a
nRaizes :: Float -> Float -> Float -> Int
nRaizes a b c 
   |delta > 0 = 2
   |delta == 0 = 1
   |delta < 0 = 0
   where delta = b^2-4*a*c

--b
raizes :: Float -> Float -> Float -> [Float]
raizes a b c
    | nRaizes a b c == 0 = []
    | nRaizes a b c == 1 = [x]
    | nRaizes a b c == 2 = [x1,x2]
    where delta = b^2 - 4*a*c
          x = (-b) / (2*a)
          x1 = ((-b) + sqrt delta) / (2*a)
          x2 = ((-b) - sqrt delta) / (2*a)

---8
--a
isLower' :: Char -> Bool
isLower' c = ord c >= 97 && ord c <= 122

--d
toUpper' :: Char -> Char 
toUpper' c = if isLower' c then chr ((ord c)-32) else c

--e
intToDigit' :: Int -> Char
intToDigit' c = chr (c+48)

--f
digitToInt' :: Char -> Int
digitToInt' c = ord c - 48 












