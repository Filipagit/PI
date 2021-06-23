
--exercicio 1 
data BTree a = Empty
| Node a (BTree a) (BTree a)
deriving Show

--a calcula a altura da arvore conseguiiiii
altura :: BTree a -> Int 
altura Empty = 0 
altura (Node x e d ) = 1+ max (altura e)(altura d)

--b  calcula o nr de nodos de uma arvore conseguiiiii
contaNodos :: BTree a -> Int 
contaNodos Empty = 0
contaNodos (Node x e d) = 1+ (contaNodos e) + (contaNodos d)

--c calcula o nr de folhas(nodos sem descendente) conseguiiiii
folhas :: BTree a -> Int 
folhas Empty = 0
folhas (Node x Empty Empty) = 1
folhas (Node x e d) =  folhas e + folhas d

--d remove tds os elementos a partir de uma dada profundidade conseguiiii
prune :: Int -> BTree a -> BTree a 
prune 0 _ = Empty
prune _ Empty = Empty
prune n (Node x e d)= Node x(prune(n-1) e)(prune(n-1) d)

--e False esq True dir conseguiiii
path :: [Bool] -> BTree a -> [a]
path [] _ = []
path _ Empty = []
path (h:t)(Node x e d) |h==False = x:(path t e) 
                       |otherwise= x:(path t d)

--f 
mirror :: BTree a -> BTree a 
mirror Empty = Empty
mirror (Node x e d) = Node x (mirror d)(mirror e)                       

--g  conseguiiii
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c 
zipWithBT f (Node x e d) (Node n l r) = Node (f x n) (zipWithBT f e l) (zipWithBT f d r)
zipWithBT f _ _ = Empty

--h
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (x,y,z) e d) = ((Node x a1 a2), (Node y b1 b2), (Node z c1 c2))
             where (a1,b1,c1) = unzipBT e 
                   (a2,b2,c2) = unzipBT d 

---exercicio 2

--a calcula o menor elemento de uma arvore binaria de procura nao vazia conseguiii
minimo :: Ord a => BTree a -> a 
minimo (Node x Empty _) = x 
minimo (Node x e d) = minimo e                    

--b remove o menor elemento 
semMinimo :: Ord a => BTree a -> BTree a 
semMinimo (Node x Empty d)= d
semMinimo (Node x e d)= Node x (semMinimo e) d

--c faz a alinea a) e b) conseguiiiii
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) =(x,d)
minSmin (Node x e d)= (e1,Node x e2 d)
	        where(e1,e2)=minSmin e

--d remove um elemento usando a minSmin
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

--exercicio 3 
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
| Rep
| Faltou
deriving Show
type Turma = BTree Aluno -- arvore binaria de procura (ordenada por numero)

--a verifica se um aluno com um dado numero esta inscrito conseguiiii
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum num (Node(n,_,_,_)e d)| n ==num  = True 
                              | n < num = inscNum num d
                              | n > num =inscNum num e 
                              

--b verifica se um aluno , com um dado nome esta inscrito conseguiii
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome nome (Node(_,no,_,_)e d)=nome == no || inscNome e || inscNome d 

--c lista o nr e o nome dos alunos trabest
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node(nu,no,reg,_)e d)= if reg== TE
	                            then trabEst e ++ [(nu,no)] ++ trabEst d  
	                            else trabEst e ++ trabEst d 

--d calcula a classificação de um aluno conseguiii
nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota n (Node(nu,_,_,cl)e d) |n ==nu = Just cl
                            |nu > n = nota n e 
                            |nu < n = nota n d 

--e calcula a percentagem de alunos que faltaram a avaliação conseguiii
percFaltas :: Turma -> Float 
percFaltas Empty = 0
percFaltas turma  = sumfalt turma /  numtotA
       where sumfalt Empty =0
             sumfalt(Node(_,_,_,cl)e d) = if cl==Faltou
                                          then 1 + sumfalt  e + sumfalt d 
                                          else sumfalt e + sumfalt d
        numtotA Empty = 0
        numtotA (Node x e d) = 1 + numtotA e + numtotA d 

--f calcula a media dos alunos que passaram conseguiiii
mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov turma = sumAprov/numAprov
    where sumAprov Empty = 0
         sumAprov(Node(_,_,_,cl)e d)= if cl == Aprov x
                                        then fromIntegral x + sumAprov e + sumAprov d 
                                        else sumAprov e + sumAprov d 

             numAprov Empty =0
             numAprov (Node(_,_,_,cl)e d)= if cl == Aprov x 
                                           then 1 + numAprov e + numAprov d 
                                           else numAprov d + numAprov e 

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




