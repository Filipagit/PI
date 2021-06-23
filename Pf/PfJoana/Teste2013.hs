module Teste2013 where

import Data.List

----Parte I
---1
merge' :: Ord a => [a] -> [a] -> [a]
merge' l [] = l 
merge' [] l = l
merge' (x:xs) (y:ys) = if x<y then x: merge' xs (y:ys)
                              else y: merge' (x:xs) ys

---2
triplos :: [a] -> [(a,a,a)]
triplos [] = []
triplos [x] = []
triplos [x,y] = []
triplos (x:y:z:xs) = (x,y,z): triplos xs

---3
fun :: Num a => [a] -> [a]
fun l = map (*2) l

fun2 :: Num a => [a] -> [a]
fun2 [] = []
fun2 (x:xs) = (2*x): fun2 xs 

---4
type Filme = (Titulo,Realizador,[Actor],Ano,Duracao)
type Titulo = String
type Realizador = String
type Actor = String
type Ano = Int
type Duracao = Int

filmes :: [Filme]
filmes = [("Hello","ab",["bc","cd"],2018,120),("World","df",["ef","fg","gh"],2019,200)]

--a
doActor :: Actor -> [Filme] -> [(Titulo,Ano)]
doActor _ [] = []
doActor actor ((t,r,la,y,d):xs) = if elem actor la then (t,y): doActor actor xs
                                                   else doActor actor xs 

--b
total :: [Titulo] -> [Filme] -> Int
total [] _ = 0
total _ [] = 0 
total (x:xs) filmes = procuraFilme x filmes + total xs filmes 

procuraFilme :: Titulo -> [Filme] -> Int
procuraFilme _ [] = 0 
procuraFilme titulo ((t,_,_,_,d):xs) = if titulo==t then d else procuraFilme titulo xs

---5
data LTree a = Leaf a | Fork (LTree a) (LTree a)

select :: LTree a -> [Bool] -> (Maybe a)
select (Leaf x) [] = Just x 
select (Leaf x) xs = Nothing
select (Fork x y) [] = Nothing
select (Fork x y) (True:xs) = select y xs
select (Fork x y) (False:xs) = select x xs 

----Parte II
---1
--a
procura :: Eq a => LTree a -> a -> Maybe [Bool]
procura (Leaf x) n = if n==x then Just []
                             else Nothing
procura (Fork x y) n | left == Nothing && right == Nothing = Nothing
                     | left == Nothing = Just (True:r)
                     | right == Nothing = Just (False:l)
                   where left = procura x n 
                         right = procura y n 
                         Just l = left
                         Just r = right

 



