--1 constroi uma lista dos  numeros inteiros compreendidos entre 2 limites 
myenumFromTo :: Int -> Int -> [Int]
myenumFromTo x n = if(x<n)
	                then x:myenumFromTo (x+1) n
	                else [n]

--2 constroi uma lista dos numeros inteiros compreendidos entre 2 limites espaçados de um valor constante 
myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo k n x
              |k<n && k<x = k: myenumFromThenTo n (n+(n-k)) x                
              |k==x = [k]
              |k==n = repeat k
              |otherwise = []

--3 concatena duas listas conseguiii
concatena :: [a] -> [a] -> [a]
concatena [] l = l
concatena (x:xs)(h:t)= x:concatena xs(h:t)

--4 !! dada uma lista e um inteiro calcula o elemento dessa lista que se encontra nessa posição conseguiii
calcula :: [a] -> Int -> a
calcula [] _ = error "empty list"
calcula (h:t) x = if(x==0) then h 
	              else calcula t (x-1)

--5 reverte uma lista conseguiii
reverte :: [a] -> [a]
reverte [] = [] 
reverte (h:t) = reverte t ++ [h] 

--6 dado um inteiro n e uma lista l calcula os no max n primeiros elementos de l conseguiii 
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake 0 l = []
mytake x (h:t) =  h:(mytake (x-1) t) 

--7 dado um inteiro n e uma lista l calcula a lista sem os no max n primeiros elementos de l conseguiii
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop 0 l = l 
mydrop n (h:t) = mydrop (n-1) t      

--8 conseguiii
myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs)(h:t) = (x,h) : myzip xs t 

--9 conseguiiii
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False 
myelem n (h:t)= if(n==h)
	            then True 
	            else myelem n t 

--10 dado um inteiro n e um elemento x constroi uma lista com n elementos todos iguais a x conseguiiii 
myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = x: myreplicate (n-1) x 

--11 conseguiiii
myintersperce :: a -> [a] -> [a]
myintersperce _ [x] = [x]
myintersperce n (h:t) = h:n : myintersperce n t 

--12 agrupa elementos iguais consecutivos
mygroup :: Eq a => [a] -> [[a]]
mygroup [] = [[]]
mygroup (h:t) = aux [h] t
  where
    aux a [] = [a]
    aux a (h:t)
      | elem h a = aux (h : a) t
      | otherwise = a : aux [h] t

--13 concatena listas de uma lista conseguiii
myconcat :: [[a]] -> [a]
myconcat [[]] = []
myconcat [[x]] = [x]
myconcat (h:t) = h ++ myconcat t

--14 init [1,2,3] -[1,2]
myinits :: [a] -> [[a]]
myinits [] = [[]]
myinits l = myinits(init l) ++ [l]

--15 conseguiiii
mytails :: [a] -> [[a]]
mytails [] = [[]]
mytails l = [l] ++ mytails(tail l)

--16 conseguiiii
myisPrefixOf :: Eq a => [a] -> [a] -> Bool
myisPrefixOf [] _ = True 
myisPrefixOf _ [] = False
myisPrefixOf (x:xs) (h:t) = if x==h 
	                        then myisPrefixOf xs t 
	                        else False 
--50
data Semaforo = Verde | Amarelo | Vermelho 
              deriving Show 

myintersecaoOK :: [Semaforo] -> Bool
myintersecaoOK l = if (contavermelhos l <=1)
                 then True 
                 else False  

contavermelhos :: [Semaforo] -> Int 
contavermelhos [] = 0
contavermelhos (Vermelho:t) = 1+ contavermelhos t
contavermelhos (_:t) = contavermelhos t  
	                   

