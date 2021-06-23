module Exame2009 where

---1 
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (x:y:xs) = y: posImpares xs 

---2
repetidos :: Eq a => [a] -> Bool
repetidos [] = False
repetidos (x:xs) = elem x xs || repetidos xs 

---3
data Tree a = Empty | Node a (Tree a) (Tree a)

--a
somaNum :: Int -> Tree Int -> Tree Int
somaNum _ Empty = Empty
somaNum n (Node x e d) = Node (x+n) (somaNum n e) (somaNum n d)

---4
multip :: Int -> [(Int,Int,Int)] -> [(Int,Int,Int)]
multip _ [] = []
multip n ((x,y,z):xs) = if (mod (x+y+z) n) == 0 then (x,y,z) : multip n xs 
                                                else multip n xs 

---5
type TabAbrev = [(Abreviatura,Palavra)]
type Abreviatura = String
type Palavra = String

--a
daPal :: TabAbrev -> Abreviatura -> Maybe Palavra
daPal [] _ = Nothing
daPal ((ab,pal):xs) abrv = if ab==abrv then Just pal 
                                       else daPal xs abrv

--b
transforma :: TabAbrev -> String -> String
transforma t s = unwords (trata t (words s))

trata :: TabAbrev -> [String] -> [String]
trata tab (x:xs) = (trataAbrv tab x) : trata tab xs   
trata _ _ = []

trataAbrv :: TabAbrev -> String -> String
trataAbrv ((ab,pal):xs) abrv = if ab==abrv then pal 
                                           else trataAbrv xs abrv 
trataAbrv _ _ = []

----ParteII
---1
{--participou :: String -> [(String, Int)]
participou [] = []
participou l = words l 


filtrAlunos :: [String] -> [String]
filtrAlunos [] = []
filtrAlunos (a@(x:y:xs):ys) | x /= '-' && y/= '-' = a:filtrAlunos ys
			                | otherwise = filtrAlunos ys

contaPart :: String -> [String] -> Int
contaPart _ [] = 0 
contaPart n (x:xs) = if n==x then 1 + contaPart xs 
                             else contaPart xs 
--}

---2
data AG = Pessoa Nome Pai Mae
        | Desconhecida
    deriving Show
type Pai = AG
type Mae = AG
type Nome = String

agen :: AG 
agen = Pessoa "Ines" (Pessoa "Joao" (Pessoa "Manuel" Desconhecida Desconhecida) (Pessoa "Maria" Desconhecida Desconhecida)) (Pessoa "Ana" (Pessoa "Ze" Desconhecida Desconhecida) (Pessoa "Mariana" Desconhecida Desconhecida))

--a
nomesMF :: AG -> ([Nome],[Nome])
nomesMF Desconhecida = ([],[])
nomesMF (Pessoa nome pai mae) = (x1++x2,y1++y2)
                             where (x1,y1) = nomesP pai 
                                   (x2,y2) = nomesM mae

nomesP :: AG -> ([Nome],[Nome])
nomesP Desconhecida = ([],[])
nomesP (Pessoa nome pai mae) = (nome:x1++x2,y1++y2)
                           where (x1,y1) = nomesP pai 
                                 (x2,y2) = nomesM mae  

nomesM :: AG -> ([Nome],[Nome])
nomesM Desconhecida = ([],[])
nomesM (Pessoa nome pai mae) = (x1++x2,nome:y1++y2)
                            where (x1,y1) = nomesP pai 
                                  (x2,y2) = nomesM mae

--b
{--avos :: Nome -> AG -> [Nome]
avos _ Desconhecida = []
avos n (Pessoa nome (Pessoa pai pai2 mae2) (Pessoa mae pai3 mae3)) = if n==nome then [] 
--}

--c
grau :: AG -> Nome -> Maybe Int
grau Desconhecida _ = Nothing
grau a n = Just (grauAux a n 0)  

grauAux :: AG -> Nome -> Int -> Int 
grauAux (Pessoa n p m) nome g = if nome==n then g else max f g 
                             where f = grauAux p nome (g+1)
                                   g = grauAux m nome (g+1)  


 
