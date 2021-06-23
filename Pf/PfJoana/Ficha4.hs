module Ficha4 where
import Data.Char (isDigit,isAlpha)
import Data.List (inits)

---1
--a [6,12,18]
--b [6,12,18]
--c [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]
--d

---2
--a [2^x | x <- [0..10]]
--b [(x,y) | x <- [1..5], y <- [1..5], x+y==6]
--c [[1..x] | x <- [1..5]]
--d [replicate x 1 | x <- [1..5]]
--e [product [1..x] | x <- [1..6]]

---3
digitAlpha :: String -> (String,String)
digitAlpha [] = ("","")
digitAlpha (x:xs) | isDigit x = (x:a,b)
                  | isAlpha x = (a,x:b)
                  | otherwise = (a,b)
     where (a,b) = digitAlpha xs   

---4
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h<0 = (n+1,z,p)
          | h==0 = (n,z+1,p)
          | h>0 = (n,z,p+1)
     where (n,z,p) = nzp t 

---5
divMod' :: Integral a => a -> a -> (a, a)
divMod' x y = if x>y 
                  then (a+1,b) 
                  else (0,x)
      where (a,b) = divMod' (x-y) y 

---6
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t 

-----------------------------------------------------

myfromDigits :: [Int] -> Int
myfromDigits [] = 0
myfromDigits x = aux (length x-1) x  
  where aux :: Int -> [Int] -> Int
        aux 0 [x] = x
        aux n (x:xs) = x*10^n + aux (n-1) xs 

---7
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l]

-----------------------------------------------------

maxSumInit2 :: (Num a, Ord a) => [a] -> a
maxSumInit2 l = sum l2 
   where l2= maxAC (inits l) 

maxAC :: (Num a, Ord a) => [[a]] -> [a]
maxAC (x:y:xs) = if (sum x) > (sum y)
                 then maxAC (y:xs)
                 else maxAC (x:xs)
maxAC [x] = x 

---8
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-----------------------------------------------------
---ERRADA
myfib :: Int -> Int
myfib n = auxfib 0 1 n 

auxfib :: Int -> Int -> Int -> Int
auxfib n1 n2 n | (n2==n-1) = n1+n2
               | otherwise = auxfib n2 (n1+n2) n 


