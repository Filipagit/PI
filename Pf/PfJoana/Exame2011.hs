module Exame2011 where

----ParteI
---2
data BTree a = Vazia | Nodo a (BTree a) (BTree a)

arv :: BTree Float
arv = Nodo 5 (Nodo 3 (Nodo 2 (Nodo 1 Vazia Vazia) Vazia) (Nodo 4 Vazia Vazia)) (Nodo 10 (Nodo 7 Vazia (Nodo 8 Vazia Vazia)) (Nodo 12 Vazia Vazia))

--a
limites :: BTree a -> (a,a)
limites arv = (a,b) 
          where a = menorElemento arv 
                b = maiorElemento arv  

menorElemento :: BTree a -> a 
menorElemento (Nodo x Vazia _) = x 
menorElemento (Nodo x e _) = menorElemento e

maiorElemento :: BTree a -> a 
maiorElemento (Nodo x _ Vazia) = x 
maiorElemento (Nodo x _ d) = maiorElemento d 

--b
media :: BTree Float -> Float
media Vazia = 0
media arv = somaEl arv / totalEl arv

somaEl :: BTree Float -> Float
somaEl Vazia = 0
somaEl (Nodo x e d) = x + somaEl e + somaEl d  

totalEl :: BTree Float -> Float
totalEl Vazia = 0
totalEl (Nodo x e d) = 1 + totalEl e + totalEl d 

