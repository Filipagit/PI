module Ficha5 where

---1
--a
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) = f x || any' f xs 

--b
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys 
zipWith' f _ _ = []

--c
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) = if f x then x:takeWhile' f xs 
                             else [] 

--d
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) = if f x then dropWhile' f xs 
                             else (x:xs)

--e
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (x:xs) = if f x then (x:a,b)
                        else ([],x:xs)
   where (a,b) = span' f xs

--f
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f _ [] = []
deleteBy' f n (x:xs) = if f n x then xs
                                else x:deleteBy' f n xs 

--g
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (x:xs) = insert' f x (sortOn' f xs)
    where insert' :: Ord b => (a -> b) -> a -> [a] -> [a]
          insert' f x [] = [x]
          insert' f x (h:t) = if (f x) < (f h) then x:h:t 
                                               else h:insert' f x t 

---2
type Polinomio = [Monomio]
type Monomio = (Float,Int)
--a
selgrau :: Int -> Polinomio -> Polinomio
selgrau g p = filter (\(c,e) -> e==g) p  

--b
conta :: Int -> Polinomio -> Int
conta g p = foldr (\(c,e) s -> if e==g then s+1 else s) 0 p

--c
grau :: Polinomio -> Int
grau ((c,g):p) = foldr (\(_,g2) g1 -> max g2 g1) g p 

--d
deriv :: Polinomio -> Polinomio
deriv p = let l = map (\(c,g) -> (c*fromIntegral g, g-1)) p 
          in filter (\(c,_) -> c/=0) l 

--e
calcula :: Float -> Polinomio -> Float
calcula x p = foldr (\(c,g) s -> c*(x^g)+s) 0 p 

--f
simp :: Polinomio -> Polinomio 
simp p = filter (\(c,g) -> c/=0) p 

--g
mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) = map (\(b,e) -> (b*x,y+e)) 

--h
ordena :: Polinomio -> Polinomio
ordena = sortOn' (snd)

--i
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((c,g):t) = (x,g): normaliza lgd 
   where lgi = filter (\(_,g1) -> g==g1) ((c,g):t)
         lgd = filter (\(_,g1) -> g/=g1) t 
         x = sum (map fst lgi)

--j
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = foldr aux p1 p2 
    where aux :: Monomio -> Polinomio -> Polinomio
          aux (cm,gm) ((c,g):t) = if gm==g then (cm+c,gm):t
                                           else (c,g):aux (cm,gm) t 

--k 
produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = foldr mult p1 p2 

--l 
--equiv :: Polinomio -> Polinomio -> Bool
--equiv p1 p2 = all (\p -> ordena )

---3
type Mat a = [[a]]

--a
dimOK :: Mat a -> Bool
dimOK (h:t) = all (\h1 -> length h == length h1) t 

--b
dimMat :: Mat a -> (Int,Int)
dimMat (h:t) = (length h, length(h:t))

--c
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat m1 m2 = zipWith (\l1 l2 -> zipWith (+) l1 l2) m1 m2 

--d
transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = l: transpose rm 
   where l= map head m 
         rm = map tail m 

--e
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = zipWith (\l1 l2 -> zipWith (*) l1 l2) m1 m2 

--f
zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f m1 m2 = zipWith (\l1 l2 -> zipWith f l1 l2) m1 m2 

--g
triSup :: (Num a,Eq a) => Mat a -> Bool
triSup [] = True
triSup (h:t) = all (==0) l && triSup rm
    where l = map head t 
          rm = map tail t  

--h
rotateLeft :: Mat a -> Mat a
rotateLeft ([]:_) = []
rotateLeft m = l: rotateLeft rm 
   where l = map last m 
         rm = map init m 