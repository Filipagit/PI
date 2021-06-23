module Teste2008 where

---1
parte :: Int -> [Int] -> ([Int],[Int],[Int])
parte _ [] = ([],[],[])
parte n (x:xs) | x<n = (x:a,b,c)
               | x==n = (a,x:b,c)
               | otherwise = (a,b,x:c)
             where (a,b,c) = parte n xs

---2
merge :: [Float] -> [Float] -> [Float]
merge l [] = l 
merge [] l = l 
merge (x:xs) (y:ys) = if x<y then x: merge xs (y:ys)
                             else y: merge (x:xs) ys 

---3
data ABin a = Vazia | No a (ABin a) (ABin a) deriving Show 

arv :: ABin Int 
arv = No 4 (No 2 (No 1 Vazia Vazia) (No 3 Vazia Vazia)) (No 5 Vazia (No 6 Vazia Vazia))

semMin :: ABin Int -> (Int, ABin Int)
semMin arv = (minA arv, tiraMin arv)

minA :: ABin Int -> Int 
minA (No x Vazia _) = x 
minA (No x e d) = minA e 

tiraMin :: ABin Int -> ABin Int 
tiraMin Vazia = Vazia
tiraMin (No x Vazia Vazia) = Vazia 
tiraMin (No x Vazia d) = d 
tiraMin (No x e d) = No x (tiraMin e) d

---4
type Filme = (Titulo,Realizador,[Actor],Genero,Ano)
type Titulo = String
type Realizador = String
type Actor = String
type Ano = Int

data Genero = Comedia | Drama | Ficcao | Accao | Animacao | Documentario
        deriving Eq
type Filmes = [Filme]

--a
doRealizador :: Filmes -> Realizador -> [Titulo]
doRealizador [] _ = []
doRealizador ((t,r,_,_,_):xs) rlz = if rlz==r then t:doRealizador xs rlz
                                              else doRealizador xs rlz 

--b
doActor :: Filmes -> Actor -> [Titulo]
doActor [] _ = []
doActor ((t,_,la,_,_):xs) ator = if elem ator la then t:doActor xs ator
                                                 else doActor xs ator 

--c
consulta :: Filmes -> Genero -> Realizador -> [(Ano, Titulo)]
consulta bd gen rea = map aux (filter (teste gen rea) bd)
   where teste :: Genero -> Realizador -> Filme -> Bool
         teste g r (_,x,_,y,_) = g==y && r==x
         aux :: Filme -> (Ano,Titulo)
         aux (t,_,_,_,a) = (a,t)

----ParteII
data Avaliacao = NaoVi
               | Pontos Int
type FilmesAval = [(Filme,[Avaliacao])]

---1
{--instance Ord Avaliacao where
    (Avaliacao Pontos a1) > (Avaliacao Pontos a2) = a1>a2 


---2
grafico :: Titulo -> FilmesAval -> String
grafico _ [] = ""
grafico t (((tit,_,_,_,_),((Pontos i):xs)):ys) = if t==tit then 
--}


