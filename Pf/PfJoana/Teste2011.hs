module Teste2011 where

----ParteI
---1
type MSet a = [(a,Int)]

--a
melem :: Ord a => a -> MSet a -> Bool
melem x m = not (null (filter (aux x) m))
       where aux x (y,z) = x==y

--b
melem2 :: Ord a => a -> MSet a -> Bool
melem2 _ [] = False
melem2 n ((x,y):xs) = n==x || melem2 n xs

--c
{--mfilter :: (a -> Bool) -> MSet a -> MSet a
mfilter f [] = []
mfilter f ((x,y):xs) = if f (x,y) then (x,y):mfilter f xs 
                                  else mfilter f xs 
--}
--d
media :: MSet Float -> Float 
media l = soma l / todos l 

soma :: MSet Float -> Float 
soma [] = 0 
soma ((x,y):xs) = (x*fromIntegral y) + soma xs 

todos :: MSet Float -> Float 
todos [] = 0 
todos ((x,y):xs) = fromIntegral y + todos xs 

---2
data From a = Last a | Next (From a)

--a
topair :: From a -> (a,Int)
topair (Last x) = (x,0)
topair (Next x) = (a,b+1)
             where (a,b) = topair x

--b
instance Show a => Show (From a) where 
     show (Last x) = show x 
     show (Next x) = "+" ++ show x

----ParteII
---1
data List a b = Nil | ConsA a (List a b) | ConsB b (List a b) deriving Show 

--a
unzipAB :: List a b -> ([a],[b])
unzipAB Nil = ([],[])
unzipAB (ConsA x xs) = (x:a,b)
                   where (a,b) = unzipAB xs
unzipAB (ConsB x xs) = (a,x:b)
                   where (a,b) = unzipAB xs

--b
{--sortA :: (Ord a) => List a b -> List a b
sortA Nil = Nil
sortA (ConsA x xs) = insere x (sortA xs)
sortA (ConsB x xs) = 

insere :: (Ord a) => a -> List a b -> List a b 
insere n Nil = ConsA n (Nil)
insere n (ConsA x xs) | n == x = ConsA x xs 
                      | otherwise = ConsA x (insere n xs)
insere n (ConsB x xs) | n == x = ConsB x xs
                      | otherwise = 
--}

