module Teste2010 where

import Data.List
import Data.Char 

----ParteI
---1
intercala :: a -> [a] -> [a] 
intercala _ [] = []
intercala n (x:xs) = x:n:intercala n xs 

---2
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) i = if i==0 then x 
                         else (!!!) xs (i-1)

---3
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs 
catMaybes (Just x:xs) = x:catMaybes xs 

---4
type Candidato = String
type Boletim = [Candidato] -- lista de nomes que consta nos boletins de voto
type Votacao = [Candidato] -- cada ocorrência de um candidato representa um voto
type Escrutinio = [(Candidato,Int)]

bolet :: [Candidato]
bolet = ["Ana","Diana","Diogo","Goncalo","Catarina"]

vot :: [Candidato]
vot = ["Ana","Diogo","Diogo","Diogo","Diana","Diana","Goncalo","Goncalo","Goncalo","Goncalo","Catarina","Catarina","Goncalo","Goncalo"]

--a
votos :: Candidato -> Votacao -> Int
votos _ [] = 0
votos c (x:xs) = if c==x then 1 + votos c xs 
                         else votos c xs 

--b
contagem :: Boletim -> Votacao -> Escrutinio
contagem [] _ = []
contagem _ [] = []
contagem (x:xs) l = if elem x l then constroi x l : contagem xs l 
                                else contagem xs l  

constroi :: Candidato -> Votacao -> (Candidato,Int)
constroi c [] = (c,0)
constroi c (x:xs) = if c==x then (a,b+1)
                            else (a,b)
                where (a,b) = constroi c xs

--c
estatistica :: Escrutinio -> [(Candidato,Float)]
estatistica [] = []
estatistica l@((x,y):xs) = (x,(fromIntegral y)*100/fromIntegral(total l)) : estatistica xs 

total :: Escrutinio -> Int 
total [] = 0
total ((x,y):xs) = y + total xs 

----ParteII
---1
data Questionario a = Resultado a
                    | Questao String (Questionario a) (Questionario a)

quest :: Questionario [Char]  
quest = Questao "Está inscrito no método A?" (Questao "Tem nota teórica >= 8?" (Questao "Tem T*0,7+TP*0,3>= 9,5 ?" (Resultado "Aprovado") (Resultado "Reprovado")) (Resultado "Reprovado")) (Questao "Tem nota teórica >= 9,5?" (Resultado "Aprovado") (Resultado "Reprovado"))

--a
resp :: [Bool] -> Questionario a -> Maybe a
resp [] _ = Nothing
resp _ (Resultado x) = Just x 
resp (True:xs) (Questao s e d) = resp xs e 
resp (False:xs) (Questao s e d) = resp xs d 

--b
contaRes :: Eq a => Questionario a -> Int
contaRes (Resultado _) = 1
contaRes q = length (nub (listaRes q))

listaRes :: Questionario a -> [a]
listaRes (Resultado x) = [x]
listaRes (Questao s e d) = listaRes e ++ listaRes d

--c
questiona :: Questionario a -> IO a
questiona (Resultado x) = return x  
questiona (Questao s e d) = do quest1 <- putStrLn ((show s) ++ " " ++ "True or False")
                               resp <- getLine
                               case resp of "True" -> questiona e 
                                            "False" -> questiona d

---2
data List a = List Int (Int -> a)

--a
fromList :: (List a) -> [a]
fromList (List 0 f) = []
fromList (List i f) = reverse ((f i) : fromList (List (i-1) f))






