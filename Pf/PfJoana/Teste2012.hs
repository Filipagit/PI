module Teste2012 where 

----ParteI
---1
maxSumPair :: [(Int,Int)] -> (Int,Int)
maxSumPair [(x,y)] = (x,y)
maxSumPair ((x,y):(a,b):xs) = if x+y>a+b then maxSumPair ((x,y):xs)
                                         else maxSumPair ((a,b):xs)

---2
maxes :: [(Int,Int)] -> (Int,Int)
maxes [(x,y)] = (x,y)
maxes ((x,y):xs) | x>a && y<b = (x,b)
                 | x<a && y>b = (a,y)
                 | x>a && y>b = (x,y)
                 | otherwise = (a,b)
               where (a,b) = maxes xs

---3
data BTree a = Empty | Node a (BTree a) (BTree a)

procura :: Eq a => a -> BTree a -> Bool
procura n Empty = False
procura n (Node x e d) = n==x || procura n e || procura n d 

---4
concatMaper :: (a -> [b]) -> [a] -> [b]
concatMaper f [] = []
concatMaper f (x:xs) = f x ++ concatMaper f xs 

---5
type Concorrentes = [(Nu, String)] -- número e nome
type Nu = Int
type Prova = [(Nu, Float)] -- número e tempo gasto na prova

--a
junta :: Prova -> Concorrentes -> [(Nu,String,Float)]
junta ((num,tempo):xs) l = if elem num (map fst l) then juntaTudo (num,tempo) l : junta xs l  
                                                    else junta xs l   
junta _ _ = []

juntaTudo :: (Nu,Float) -> Concorrentes -> (Nu,String,Float)
juntaTudo (num,tempo) ((nr,nome):xs) = if num==nr then (nr,nome,tempo)
                                                  else juntaTudo (num,tempo) xs

--b
quantos :: Float -> Prova -> Int
quantos x p = length (filter (aux x) p)

aux :: Float -> (Int,Float) -> Bool
aux t (x,y) = y<t 

----ParteII 
data Cmd = RD | RE | AV
     deriving (Eq,Show)

---1
--i
type Pos = (Ponto,Direcao)
type Ponto = (Int,Int)
data Direcao = N | S | E | O deriving Show 

--ii
next :: Pos -> Cmd -> Pos
next ((x,y),N) cmd = case cmd of 
                     RD -> ((x,y),E)
                     RE -> ((x,y),O)
                     AV -> ((x,y+1),N)
next ((x,y),S) cmd = case cmd of
                     RD -> ((x,y),O)
                     RE -> ((x,y),E)
                     AV -> ((x,y-1),S)
next ((x,y),E) cmd = case cmd of
                     RD -> ((x,y),S)
                     RE -> ((x,y),N)
                     AV -> ((x+1,y),E) 
next ((x,y),O) cmd = case cmd of
                     RD -> ((x,y),N)
                     RE -> ((x,y),S)
                     AV -> ((x-1,y),O)

---2
percurso :: Pos -> [Cmd] -> [Pos]
percurso p [] = []
percurso p (x:xs) = (next p x) : percurso (next p x) xs 

---3
verificaColisao :: Pos -> Pos -> [Cmd] -> [Cmd] -> Bool
verificaColisao p q c d = haCol sp sq  
              where pp = percurso p c 
                    qp = percurso q d 
                    sp = retiraOr pp
                    sq = retiraOr qp

haCol :: [Ponto] -> [Ponto] -> Bool
haCol [h] l = elem h l 
haCol l [h] = elem h l 
haCol (x:xs) (y:ys) = x==y || haCol xs ys 
haCol _ _ = False

retiraOr :: [Pos] -> [Ponto]
retiraOr [] = []
retiraOr (((x,y),d):xs) = (x,y) : retiraOr xs 
 
