module Teste2018 where

---1
insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (x:xs) = if n<x then n:x:xs else x:insert' n xs 

---2
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Nothing:xs) = catMaybes' xs 
catMaybes' (Just x:xs) = x:catMaybes' xs 

---3
data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)
           
instance Show a => Show (Exp a) where
    show (Const a) = show a 
    show (Var a) = a
    show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")" 
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

---4
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (x:xs) = insert' f x (sortOn' f xs)
    where insert' :: Ord b => (a -> b) -> a -> [a] -> [a]
          insert' f x [] = [x]
          insert' f x (h:t) = if (f x) < (f h) then x:h:t
                                               else h: insert' f x t

---5
--a
amplitude :: [Int] -> Int
amplitude [] = 0 
amplitude (x:xs) = maxA - minA
               where (minA,maxA) = amplitudeAux xs (x,x) 

amplitudeAux :: [Int] -> (Int,Int) -> (Int,Int)
amplitudeAux [] x = x
amplitudeAux (h:t) (x,y) | h<x = amplitudeAux t (h,y)
                         | h>y = amplitudeAux t (x,h)
                         | otherwise = amplitudeAux t (x,y)

--b
{--parte :: [Int] -> ([Int],[Int])
parte l = parteaux (sort l)

parteaux :: [Int] -> ([Int],[Int])
parteaux 

ampCalc :: ([Int],[Int]) -> Int
ampCalc (a,b) = amplitude a + amplitude b 
--}
---6
data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5), Quadrado 4, Mover (4,3) (Quadrado 2)])

--a
conta :: Imagem -> Int
conta (Quadrado _) = 1 
conta (Mover (_,_) im) = conta im 
conta (Juntar l) = sum (map conta l) 

--b
apaga :: Imagem -> IO Imagem
apaga im = do rNum <- randomRIO (1, length indQ)
              where indQ = indicesQuadrados im 
              return (apagaIndice indQ im)   

indicesQuadrados :: Imagem -> [Int] 
indicesQuadrados (Quadrado n) = [n]
indicesQuadrados (Mover (_,_) im) = indicesQuadrados im 
indicesQuadrados (Juntar l) = concatMap indicesQuadrados l 

apagaIndice :: [Int] -> Imagem -> Imagem
apagaIndice x (Quadrado n) = if n==x then Juntar [] else Quadrado n 
apagaIndice x (Mover (a,b) im) = Mover (a,b) (apagaIndice x im)
apagaIndice x (Juntar l) = Juntar (map (apagaIndice x) l)  


