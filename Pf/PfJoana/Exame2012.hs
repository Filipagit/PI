module Exame2012 where

----ParteI
---1
posNeg :: [Int] -> (Int,Int)
posNeg [] = (0,0)
posNeg (x:xs) | x>=0 = (a+1,b)
              | x<0 = (a,b+1)
              | otherwise = (a,b) 
            where (a,b) = posNeg xs

---2
tiraPrefixo :: String -> String -> Maybe String
tiraPrefixo [] l = Just l
tiraPrefixo _ [] = Nothing
tiraPrefixo (x:xs) (y:ys) = if x==y then Just l  
                                    else Nothing
                       where Just l = tiraPrefixo xs ys 

---3
fun f l = product (map f (filter (>0) l))

fun2 f [] = 1
fun2 f (x:xs) = if x>0 then (f x) * fun2 f xs 
                       else fun2 f xs

---4
data ArvBin a = Vazia | Nodo a (ArvBin a) (ArvBin a) deriving Show 

insere :: Ord a => a -> ArvBin a -> ArvBin a
insere n Vazia = Nodo n Vazia Vazia
insere n (Nodo x e d) | n<x = Nodo x (insere n e) d 
                      | n>x = Nodo x e (insere n d)
                      | n==x = Nodo x e d 

arv :: ArvBin Integer
arv = Nodo 5 (Nodo 3 (Nodo 2 (Nodo 1 Vazia Vazia) Vazia) (Nodo 4 Vazia Vazia)) (Nodo 10 (Nodo 7 Vazia (Nodo 8 Vazia Vazia)) (Nodo 12 Vazia Vazia))

---5
type Concorrentes = [(Nu, String)] -- número e nome
type Nu = Int
type Prova = [(Nu, Float)] -- número e tempo gasto na prova

conc:: Concorrentes
conc = [(1,"Joana"),(2,"Zé"),(3,"Inês"),(4,"Gonçalo")]

prov:: Prova
prov= [(1,15.2),(2,14.0),(3,10.1),(4,14.0)]

--a
nomes :: Prova -> Concorrentes -> [(String,Float)]
nomes ((num,tempo):xs) l = if elem num (map fst l) then associaNome (num,tempo) l : nomes xs l 
                                                   else nomes xs l 
nomes _ _ = [] 

associaNome :: (Nu,Float) -> Concorrentes -> (String,Float)
associaNome (num,tempo) ((nr,nome):xs) = if num==nr then (nome,tempo) 
                                                    else associaNome (num,tempo) xs 

--b
ordena :: Prova -> Prova
ordena [] = []
ordena ((nr,tempo):xs) = insereC (nr,tempo) (ordena xs) 

insereC :: (Nu,Float) -> Prova -> Prova
insereC n [] = [n]
insereC (num,tempo) ((nr,time):xs) = if tempo<time then (num,tempo):(nr,time):xs
                                                   else (nr,time):insereC (num,tempo) xs

----ParteII
--data ArvBin a = Vazia | Nodo a (ArvBin a) (ArvBin a)

---1
camValido :: ArvBin a -> [Bool] -> Bool
camValido Vazia _ = False
camValido (Nodo x _ _) [] = True
camValido (Nodo x e d) (True:xs) = camValido d xs 
camValido (Nodo x e d) (False:xs) = camValido e xs

---2
caminho :: (Eq a) => ArvBin a -> a -> Maybe [Bool]
caminho x n = if pertence x n then Just (caminhoAux x n) 
                              else Nothing

caminhoAux :: (Eq a) => ArvBin a -> a -> [Bool]
caminhoAux Vazia _ = []
caminhoAux (Nodo x e d) n | pertence e n = False : caminhoAux e n 
                          | pertence d n = True : caminhoAux d n
                          | otherwise = [] 

pertence :: (Eq a) => ArvBin a -> a -> Bool
pertence Vazia _ = False
pertence (Nodo x e d) n | n==x = True
                        | otherwise = pertence e n || pertence d n 




