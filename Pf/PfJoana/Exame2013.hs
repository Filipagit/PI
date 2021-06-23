module Exame2013 where 

----ParteI
---1
sufixos :: [a] -> [[a]]
sufixos [] = [[]]
sufixos (x:xs) = (x:xs) : sufixos xs

---2
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) = if f x then dropWhile' f xs
                             else (x:xs) 

---3
data Alunos = Vazia | Nodo (Numero,Nome,Nota) Alunos Alunos
type Numero = Int
type Nome = String
type Nota = Int

--a
{--aprovados :: Alunos -> Int
aprovados a = let (ap,total) = conta a
              in div (ap * 100) total

conta :: Alunos -> (Int,Int)
conta Vazia = (0,0)
conta (Nodo (nr,nome,nota) Vazia Vazia) = if nota>=10 then (1,1)
                                                      else (0,1)
conta (Nodo (nr,nome,nota) Vazia d) = if nota>=10 then (a+1,b+1)
                                                  else (a,b+1)
                                   where (a,b) = conta d 
conta (Nodo (nr,nome,nota) e Vazia) = if nota>=10 then (a+1,b+1)
                                                  else (a,b+1)
                                   where (a,b) = conta e 
conta (Nodo (nr,nome,nota) e d) = if nota>=10 then (a+1,b+1)
                                               else (a,b+1)
                               where (a,b) = conta e && conta d
--}
--b
nota :: Numero -> Alunos -> Maybe Nota
nota n Vazia = Nothing
nota n (Nodo (nr,nome,nt) e d) | n<nr = nota n e 
                               | n>nr = nota n d
                               | otherwise = Just nt

---4
type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

--a
pertence :: Int -> ConjInt -> Bool
pertence _ [] = False 
pertence n ((x,y):xs) = elem n [x..y] || pertence n xs

--b
quantos :: ConjInt -> Int
quantos [] = 0 
quantos ((x,y):xs) = (y-x+1) + quantos xs

----ParteII
---2
data ArvIrr a = No a [ArvIrr a]

arvore:: ArvIrr Int 
arvore = No 2 [No 3 [No 6 []], No 4 [No 7 [], No 8 []], No 5 []] 

[(2,1),(3,2),(6,3),(4,2),(7,3),(8,3),(5,2)]

--a
maximo :: Ord a => ArvIrr a -> a
maximo n = maximum (getNodos n)

getNodos :: ArvIrr a -> [a] 
getNodos (No x l) = [x] ++ concatMap (getNodos) l 
   
