module Ficha6 where

---1
data BTree a = Empty
             | Node a (BTree a) (BTree a)
          deriving Show

--a
altura :: BTree a -> Int
altura Empty = 0
altura (Node x e d) = 1 + max (altura e) (altura d)

--b
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x e d) = 1 + contaNodos e + contaNodos d 

--c
folhas :: BTree a -> Int
folhas Empty = 0 
folhas (Node x Empty Empty) = 1 
folhas (Node x e d) = folhas e + folhas d 

--d
prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune _ Empty = Empty 
prune n (Node x e d) = Node x (prune (n-1) e) (prune (n-1) d)

--e
path :: [Bool] -> BTree a -> [a]
path [] _ = []
path _ Empty = []
path (x:xs) (Node y e d) = if x==True then y:(path xs d)
                                      else y:(path xs e)

--f
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x e d) = Node x (mirror d) (mirror e)

--g
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e1 d1) (Node y e2 d2) = Node (f x y) (zipWithBT f e1 e2) (zipWithBT f d1 d2)
zipWithBT _ _ _ = Empty

--h
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (x,y,z) e d) = ((Node x a1 a2), (Node y b1 b2), (Node z c1 c2))
             where (a1,b1,c1) = unzipBT e 
                   (a2,b2,c2) = unzipBT d 

---2
--a
minimo :: Ord a => BTree a -> a
minimo (Node x Empty _) = x 
minimo (Node x e d) = minimo e 

--b
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d 
semMinimo (Node x e d) = Node x (semMinimo e) d 

--c
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = (a,Node x b d)
   where (a,b) = minSmin e 

--d
remove :: Ord a => a -> BTree a -> BTree a 
remove _ Empty = Empty
remove n (Node x e d) | n<x = Node x (remove n e) d 
                      | n>x = Node x e (remove n d)
                      | otherwise = aux e d
           where aux :: Ord a => BTree a -> BTree a -> BTree a
                 aux Empty d = d 
                 aux e Empty = e 
                 aux e d = Node m e d' 
                         where (m,d') = minSmin d  

---3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
   deriving Show
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)

--a
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) e d) | n < num = inscNum n e 
                                 | n > num = inscNum n d 
                                 | otherwise = True 

--b 
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,nome,_,_) e d) = n==nome || inscNome n e || inscNome n d 

--c 
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nome,reg,_) e d ) = case reg of 
                                       TE -> trabEst e ++ [(num,nome)] ++ trabEst d 
                                       _ -> trabEst e ++ trabEst d

--d 
nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota n (Node (num,_,_,c) e d) | n < num = nota n e 
                              | n > num = nota n d 
                              | otherwise = Just c 

--e 
percFaltas :: Turma -> Float
percFaltas Empty = 0 
percFaltas turma = sumFaltas turma / numAlunos turma
    where sumFaltas Empty = 0 
          sumFaltas (Node (_,_,_,c) e d) = case c of 
                                           Faltou -> 1 + sumFaltas e + sumFaltas d
                                           _ -> sumFaltas e + sumFaltas d
          numAlunos Empty = 0 
          numAlunos (Node x e d) = 1 + numAlunos e + numAlunos d 

--f
mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov turma = sumNotas turma / numNotas turma
     where sumNotas Empty = 0
           sumNotas (Node (_,_,_,c) e d) = case c of 
                                           Aprov nota -> fromIntegral nota + sumNotas e + sumNotas d
                                           _ -> sumNotas e + sumNotas d  
           numNotas Empty = 0 
           numNotas (Node (_,_,_,c) e d) = case c of 
                                           Aprov nota -> 1 + numNotas e + numNotas d 
                                           _ -> numNotas e + numNotas d

--g
aprovAv :: Turma -> Float
aprovAv Empty = 0 
aprovAv turma = a / b
   where (a,b) = aux turma
         aux Empty = (0,0)
         aux (Node (_,_,_,c) e d) = case c of 
                                    Aprov nota -> (x+1,y)
                                    Rep -> (x,y+1)
                                    _ -> (x,y)
             where (x,y) = (x1+x2,y1+y2)
                   (x1,y1) = aux e 
                   (x2,y2) = aux d   

turma1 :: BTree Aluno
turma1 = Node (3,"Joana",TE,Aprov 17) (Node (2,"Goncalo",TE,Aprov 14) (Node (1,"Ana",ORD,Rep) Empty Empty) Empty) (Node (4,"Pedro",MEL,Faltou) Empty Empty)

