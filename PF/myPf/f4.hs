import Data.Char (isDigit,isAlpha)
import Data.List (inits)

--1

--1a conseguii
-- [x | x <- [1..20],mod x 2==0, mod x 3 ==0] =[6,12,18]

--1b conseguiii
-- [x | x <-[y | y <-[1..20],mod y 2 ==0], mod x 3 ==0]=[6,12,18]
	        
--1c conseguiiii
--[(x,y) | x <- [0..20], y <-[0..20], x+y==30]=	[(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]           

--1d 
--[sum [y | y <- [1..x], odd y] | x <- [1..10]]=[1,1,4,4,9,9,16,16,25,25]
           -- y=[1,3,5,7,9] 

--2 

--2a  conseguiiii
--[1,2,4,8,16,32,64,128,256,512,1024]= [2^x | x <-[0..10]]

--2b  conseguiii
-- [(1,5),(2,4),(3,3),(4,2),(5,1)] = [(x,y) | x <-[1..5],y<-[1..5],x+y==6 ]

--2c 
--[[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]] = [[1..x]|x<-[1..5]]

--2d conseguiii
--[[1],[1,1],[1,1,1],[1,1,1,1],[1,1,1,1,1]]=[replicate x 1 | x<-[1..5]]


--2e 
--[1,2,6,24,120,720]=[ product [y | y <- [1..x]] | x <- [1..6]]

--3
digitAlpha :: String -> (String,String)
digitAlpha [] = ("","")
digitAlpha (h:t) | isDigit h = (h:d,a)
                 | isAlpha h = (d,h:a)
                 | otherwise = (d,a)
               where (d,a)= digitAlpha t 
                     
--4 conseguiiiiiiiiiii
nzp :: [Int] -> (Int,Int,Int)
nzp [] =(0,0,0)
nzp (h:t) |h==0 = (n,1+z,p)
          |h<0 = (1+n,z,p)
          |otherwise = (n,z,1+p)
          where(n,z,p)=nzp t

--5 calcula o resro da div inteira, e o resto da div int por sub sucess
divMod' :: Integral a => a -> a -> (a, a)
divMod' x y = if x>y 
                  then (a+1,b) 
                  else (0,x)
      where (a,b) = divMod' (x-y) y 

--6 determina qual o nr que corresponde a uma lista de digitos
fromDigits' :: [Int] -> Int
fromDigits' [] = 0
fromDigits' (h:t) = h*10^(length t) + fromDigits' t

myfromDigits :: [Int] -> Int
myfromDigits [] = 0
myfromDigits x = aux (length x-1) x  
  where aux :: Int -> [Int] -> Int
        aux 0 [x] = x
        aux n (x:xs) = x*10^n + aux (n-1) xs

--7
maxSumInit2 :: (Num a, Ord a) => [a] -> a
maxSumInit2 l = sum l2 
   where l2= maxAC (inits l) 

maxAC :: (Num a, Ord a) => [[a]] -> [a]
maxAC (x:y:xs) = if (sum x) > (sum y)
                 then maxAC (y:xs)
                 else maxAC (x:xs)
maxAC [x] = x 

        