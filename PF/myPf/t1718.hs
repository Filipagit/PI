--1 dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista
--conseguiii
isinsert :: Ord a => a -> [a] -> [a]
isinsert x [] = [x]
isinsert n (h:t)= if n<h 
	              then n:h:t
	              else h: isinsert n t 

--2 colecciona os elementos do tipo a de uma lista. conseguiiii
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:t)= catMaybes t 
catMaybes (Just x:t) = x:catMaybes t 

--3 conseguiii
data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a) where 
     show (Const a) = show a 
     show  (Var a) = a 
     show (Mais l1 l2) ="(" ++ show l1 ++ "+" ++ show l2 ++ ")"
     show (Mult l1 l2) = "(" ++ show l1 ++ "*" ++ show l2 ++ ")"

--4 ordena uma lista comparando os resultados de aplicar uma funcao de extraccao de uma chave a cada elemento de uma lista
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (h:t)= insert' f h (sortOn f t)

insert' :: Ord b => (a->b) -> a-> [a] ->[a]
insert' f x [] = [x]
insert' f x (h:t) = if (f x) < (f h) then x:h:t
                                     else h: insert' f x t

--5
--a calcula a amplitude da lista (diferença entre maior elemento e menor elemento) conseguii
amplitude :: [Int] -> Int 
amplitude [] = 0 
amplitude l = maximo l - minimo l 

maximo :: [Int] -> Int 
maximo [] = 0
maximo [x]=x
maximo (h:xs:t)= if h>xs 
	          then maximo (h:t)
	          else maximo(xs:t)

minimo :: [Int] -> Int 
minimo [] = 0
minimo	[x]=x 
minimo (h:xs:t)= if h<xs 
              then minimo(h:t)
              else minimo(xs:t)         

--alternativa uma unica passagem na lista 
amplitude' :: [Int] -> Int
amplitude' [] = 0 
amplitude' (x:xs) = maxA - minA
               where (minA,maxA) = amplitudeAux xs (x,x) 

amplitudeAux :: [Int] -> (Int,Int) -> (Int,Int)
amplitudeAux [] x = x
amplitudeAux (h:t) (x,y) | h<x = amplitudeAux t (h,y)
                         | h>y = amplitudeAux t (x,h)
                         | otherwise = amplitudeAux t (x,y)


--6 
data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

--a conta quantos quadrados tem uma imagem 
conta :: Imagem -> Int 
conta (Quadrado _) = 1
conta (Mover(x,y) i) = conta i 
conta (Juntar l) = sum(map conta l)

--b apaga os quadrados da imagem a sorte 
apaga :: Imagem -> IO Imagem 
apaga im = do rNum <- randomRIO (1, length indQ)
              where indQ = indicesQuadrados im 
              return (apagaIndice indQ im)   

indicesQuadrados :: Imagem -> [Int] 
indicesQuadrados (Quadrado n) = [n]
indicesQuadrados (Mover (_,_) im) = indicesQuadrados im 
indicesQuadrados (Juntar l) = concatMap indicesQuadrados l 

apagaIndice :: [Int] -> Imagem -> Imagem
apagaIndice x (Quadrado n) = if n==x then Juntar [] else Quadrado n 
apagaIndice x (Mover (a,b) im) = Mover (a,b) (apagaIndice x im)
apagaIndice x (Juntar l) = Juntar (map (apagaIndice x) l)  
