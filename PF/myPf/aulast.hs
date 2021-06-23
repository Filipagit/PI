--1 recebe um numero inteiro e calcula o dobro desse numero conseguiii
dobro :: Int -> Int 
dobro x = 2 * x 

--2 recebe um par de inteiros e calcula o maior deles conseguii
maior :: (Int,Int) -> Int 
maior (x,y) = if(x>y) 
	          then x
	          else y
--3 faz o mm que a 2
maiorC :: Int -> Int -> Int
maiorC x y = if x>y
	         then x 
	         else y

--4 calcula o comprimento da lista conseguiiii
comprimento :: [a] -> Int 
comprimento [] = 0
comprimento (h:t)= 1 + comprimento t 	      

--5 alternativa a 4 
comp :: [a] -> Int 
comp [] = 0
comp l =(comp(tail l)) + 1

--6 n!=n *(n-1)! 0!=1 conseguii
fatorial :: Int -> Int 
fatorial n = if(n==0)
             then 1
             else n * fatorial (n-1)    

--7 calcula a soma dos elementos conseguii 
soma :: [Int] -> Int 
soma [] = 0
soma (h:t)= h + soma t 
     
--8 da o 1 elemento de uma lista conseguii
cabeca:: [a] -> a
cabeca (h:t)= h 
     
--9 ultimo elemento de uma lista 
ultimo :: [a] -> a
ultimo (h:[]) =h
ultimo (h:(x:xs))= ultimo (x:xs)

--10 elem testa se um elemento pertence a uma lista conseguiii 
pertence :: Eq a => a -> [a] -> Bool
pertence _ [] = False 
pertence n (h:t)= if(n==h)
                  then True 
                  else pertence n t 

--11 junta 2 listas conseguiii
junta :: [a] -> [a] -> [a]
junta [] l = l 
junta l [] = l
junta (h:t) l = h:(junta t l)

--12 conseguiiii
mzip :: [a] -> [b] -> [(a,b)]
mzip (h:t)(x:xs) = (h,x) :(mzip t xs)
mzip _ _ = []

--13 
munzip :: [(a,b)] -> ([a], [b])
munzip [] = ([],[])
munzip ((x,xs):t)=  (x:fst(munzip t),xs:snd(unzip t))

--14 alternativa 
myunzip :: [(a,b)] -> ([a], [b])
myunzip ((x,y):t)=(x:xs,y:ys)
 where (xs,ys)=myunzip t --usando o where a lista so é percorrida 1x e é mais eficiente

--15 calcula o maior elemento da lista e indica a posição
--ex: pmaior [1,3,2,7,4,8,1]==5 p=pmaior [3,2,7,4,8,1]==4  
pmaior :: Ord a => [a] -> Int 
pmaior [x] = 0
pmaior (h:t)= if(h>(calculaindice t p))
	          then 0
	          else p+1
      where p= pmaior t --vai percorrer a lista duas vezes pra ver o indice e pra ver o maior 	          

--diz o elemento que esta nessa posicao calculaindice['a','b','c'] 0 ='a' chama se !! em haskell
calculaindice :: [a] -> Int -> a 
calculaindice [] _ = error "index to large"
calculaindice (h:t) p = if(p==0)
                        then h 
                        else calculaindice t (p-1)  

--alternativa pmaioraux calcula o maior elemento da lista e a posiçao onde aparece
mypmaior :: Ord a => [a] -> Int 
mypmaior l = snd (pmaiorAux l)

pmaiorAux :: Ord a => [a] -> (a,Int)
pmaiorAux [x] =(x,0)
pmaiorAux (h:t)= if(h>m)
	             then (h,0) 
	             else (m,p+1)
	     where (m,p)=pmaiorAux t --maior elemento p posição desse elemento

          

-- 16 função que ordena uma lista  por inserção iSort[7,2,5,3,9,8]==[2,3,5,7,8,9]
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t)= insere h t'
                 where t'= iSort t --t'=[2,3,5,8,9]
               

-- insere um elemento numa lista ordenada conseguiii
insere :: Ord a => a-> [a] -> [a] 
insere x [] = [x]
insere x (h:t) = if(x<=h)
                 then(x:h:t)
                 else h:(insere x t )                 

{--alternativa ordena=minSort
ordena ::Ord a => [a] -> [a]
ordena [] = []
ordena l = h:(ordena t)
     where h= menor l--h é o menor elemento da lista 
           t= remove h l --lista l removendo o h --}

--calcula o menor elemento de uma lista conseguii
menor :: Ord a => [a] -> a 
menor [] = error "empty list"
menor [n]=n
menor (x:xs:t)= if(x<xs)
	            then menor (x:t) 
	            else menor (xs:t) 

--remove um elemento da lista (remove um elemento que existe na lista)
remove :: Eq a => a -> [a] -> [a]
remove n (h:t)= if(n==h)
	            then t 
	            else h:(remove n t)

--17 ordena(quickSort) uma lista  l=[7,2,5,3,9,8] h=7 t=[2,5,3,9,8] men=[2,5,3] mai=[9,8]
ordena :: Ord a => [a] -> [a]
ordena [] = []
ordena(h:t)= (ordena men) ++ (h:(ordena mai))
      where men= menores h t--lista dos elementos de t que sao <=h [x | x <- t, x<=h]
            mai= maiores h t  --	lista dos elementos de t que sao >h   [x | x <- t, x>h]  
            -- c isto ja n é preciso a funçao menores e maiores        
			menores :: Ord a => a -> [a] -> [a]
			menores x [] = [] 
			menores x (h:t) | h<=x = h: menores t 
				            |otherwise= menores x t 
			maiores :: Ord a => a -> [a] -> [a]
			maiores x [] = []
			maiores x (h:t) | h>x = h: maiores t 
				            |otherwise= maiores x t       

-- {x | x in N && x<10} = {1,2,3,4,5,6,7,8,9}
--definição de listas por compreensão [x|x<-[1..10],mod x 2 ==0] lista de 1  a 10 e poe os pares 
-- menMAI x l = (menores x l, maiores x l) percorre na mm 2x a lista

--alternativa 
menMAI :: Ord a => a -> [a] -> ([a],[a])
menMAI x [] = ([],[])
menMAI x (h:t)= if (h>x)
	            then (p,h:q) 
	            else (h:p,q)
        where (p,q)= menMAI x t -- so percorre uma vez

ordena2 :: Ord a => [a] -> [a]
ordena2 [] = []
ordena2(h:t)= (ordena2 men) ++ (h:(ordena2 mai))
     where(men,mai)=menMAI h t 

--mergeSort    juntalistasordenadas
mymergesort :: Ord a => [a] -> [a]
mymergesort [] = []
mymergesort [x] = [x]
mymergesort l = juntalistasordenadas l1' l2'
      where l1= take (div(length l) 2) --uma sublista de l com (aproximadamente) metade dos elementos   
            l2= drop (div(length l) 2) --sublista de l c os restantes elementos
            l1'= mymergesort l1 
            l2'= mymergesort l2
            juntalistasordenadas :: Ord a => [a] -> [a] -> [a]
            juntalistasordenadas x [] = x
            juntalistasordenadas [] x = x 
            juntalistasordenadas (x:xs) (h:t) = if(x<h)
                                                then x : juntalistasordenadas xs (h:t)
                                                else  h:juntalistasordenadas (x:xs) t

-- 35 das 50q 
-- mymenor "ana" "abel" = False mymenor "abel" "ana" = True 
mymenor :: String -> String -> Bool
mymenor [] []= False
mymenor [] l= True 
mymenor l []= False 
mymenor (x:xs) (h:t) | x==h = mymenor xs t 
                     | x>h = False 
                     | x<h=True                                                 

--
myelemIndices :: Eq a => a -> [a] -> [Int]
myelemIndices x l =  elemindicesxpto 0 x l --0 pq qd começa a posição onde estamos é 0  

-- sabe a posição em que estamos Int 
elemindicesxpto :: Eq a => Int -> a -> [a] -> [Int]
elemindicesxpto p x [] = []
elemindicesxpto p x (h:t) | x==h = p: elemindicesxpto (p+1) x t
                          | x/= h = elemindicesxpto (p+1) x t       

--sum 
soma :: Num a => [a] -> a
soma [] = 0
soma (h:t) = h + soma t 

--alternativa 
somaxpto :: Num a => a -> [a] -> a
somaxpto acc [] = acc
somaxpto acc (h:t) = somaxpto (acc+h) t 

soma2 :: Num a => [a] -> a 
soma2 l = somaxpto 0 l

--50 das 50q 
data Semaforo = Verde | Amarelo | Vermelho 
              deriving show 

myintersecaoOK :: [Semaforo] -> Bool
myintersecaoOK l = if (contavermelhos l <=1)
                 then True 
                 else False  

contavermelhos :: [Semaforo] -> Int 
contavermelhos [] = 0
contavermelhos (Vermelho:t) = 1+ contavermelhos t
contavermelhos (_:t) = contavermelhos t 

-- multiplica tds os elementos conseguiii
dobros :: [Int] -> [Int]
dobros [] = []
dobros (h:t) = (2*h) : dobros t

--alternativa
dobros2 :: [Int] -> [Int]
dobros2 l = map(2*) l

--conseguii
triplos :: [Int] -> [Int]
triplos [] =[]
triplos (h:t) = (3*h) : triplos t 

--soma mais um conseguiii
maisUm :: [Int] -> [Int]
maisUm [] = []
maisUm (h:t)= (1+h) : maisUm t 


---------------------Funções de ordem Superior 

--filter  dada uma lista selecionar apenas alguns elementos dessa lista  teste é o 1 arg (a) 
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter teste [] = []
myfilter teste (h:t) = if(teste h == True)
	                   then teste h : myfilter t 
	                   else myfilter t 

-- seleciona um prefixo 
mytakewhile :: (a->Bool) -> [a] -> [a]	                   
mytakewhile f [] = []
mytakewhile f (h:t) =  if (f h)
                       then h : mytakewhile t 
                       else []

-- deita fora um prefixo
mydropwhile :: (a->Bool) -> [a] ->[a]    
mydropwhile f [] = []
mydropwhile f (h:t) = if (f h)
                      then mydropwhile f t 
                      else (h:t)                 

-- conseguiii
myzip :: [a] -> [b] -> [(a,b)]
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (h:t) = (x,h) : myzip xs t                         

-- conseguiiii
myzipWith :: (a->b->c) -> [a] -> [b] -> [c]
myzipWith f [] _ = []
myzipWith f _ []= []
myzipWith f (x:xs) (h:t) = f x h : myzipWith f xs t	

--
type Matriz = [Linha]
type Linha = Int

--m1=[[1,2,3],[4,5,6],[7,8,9]] m2=[[3,0,4],[1,1,1],[0,0,0]]
--m3=[[4,2,7],[5,6,7],[7,8,9]]
--soma matrizes 
somaM :: Matriz -> Matriz -> Matriz
somaM [] [] = []
somaM (l:ls) (k:ks) = (somalinhas l k ) : somaM ls ks 
--alternativa 
somaM1 m1 m2 = zipWith somalinhas1 m1 m2     

somalinhas :: Linha -> Linha -> Linha
somalinhas [] [] = []
somalinhas (x:xs) (y:ys) = (x+y) : somalinhas xs ys 
--alternativa para somalinhas 
somalinhas1 l1 l2 = zipWith(+) l1 l2 

-- 
concatena :: [[a]] -> [a]
concatena [] = []
concatena(h:t) = (++) h   concatena t 

--
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t)= inserir h (iSort t)
         where inserir x [] = [x]
               inserir x (h:t) | x<=h = x:h:t
                               | x>h = h : inserir x t 

-- 
percorre :: (a->b->b) -> b ->  [a] -> b 
percorre junta v [] = v
percorre junta v (h:t)= junta  h (percorre junta v t) 

--
mymap :: (a->b) -> [a] -> [b]
mymap f [] = []
mymap f (h:t) = (f h): mymap f t


--------------------------------------------------
data Lista a = Vazia | N a (Lista a)

--conseguiii
comp :: Lista a -> Int
comp Vazia = 0
comp (N x xs)= 1 + comp xs      

--
data ABin a = Vazia | N a (ABin a) (ABin a) deriving Show

--              10
--             /  \
--            5    18
--           / \   / \
--          2   7  12  21
--             / \     / \
--            6   8   19  35

-- conseguiii
compA :: ABin a -> Int 
compA Vazia = 0
compA (N x y z)= 1+ (compA y) + (compA z)  

-- conseguiii
altura :: ABin a -> Int 
altura Vazia = 0 
altura (N r e d) = 1+ max (altura e) (altura d)   

--conseguii
mapABin :: (a -> b) -> (ABin a) -> (ABin b)
mapABin f Vazia = Vazia
mapABin f (N r e d)= (f r) (mapABin f e) (mapABin f d) 

-- quaseeee(||)
elemA :: Eq a => a -> ABin a -> Bool
elemA _ Vazia = False
elemA x (N r e d) | x==r = True
                  |otherwise = (elemA x e) || (elemA x d)   

-- conseguiiiii
elemAO :: Eq a => a -> ABin a -> Bool
elemAO _ Vazia = False 
elemAO x (N r e d) | x==r =True
                   | x < r = elemAO x e
                   |otherwise= elemAO x d 

--acrescenta um elemento a uma arvore de procura conseguiii
acrescenta :: Ord a => a -> ABin a -> ABin a   
acrescenta x Vazia = N x Vazia Vazia
acrescenta x (N r e d) | x<= r = N r (acrescenta x e) d
                       | otherwise= N r e (acrescenta x d)


--
maior :: ABin a -> a
maior (N r e empty)=r 
maior (N r e d)= maior d                      


--------------------------IO´s-----------
-- x <- getChar : x vai ter o resultado de executar getChar
--getchar: do-> seq de passos pra fzr o programa 
dialogo :: IO ()
dialogo = do putStr "Nome?" -- escrever o nome
             x <- getLine -- no x tem o nome e o getline vai retornar esse nome 
             putStr ("boa tarde" ++ x ++ "\n")

--         tam lista li,ls     
randomList :: Int -> (Int,Int) -> IO [Int]
randomList 0 _ = return []
randomList n (i,s) = do  x <- randomRIO (i,s) --x é o 1 elemento da lista, randomRIO gera um numero aleatorio nesta gama 
                         xs <- randomList (n-1) (i,s) -- gera os seguintes 
                        return (x:xs)

-- muda a ordem 
permutacao :: [a] -> IO [a]
permutacao [] = return []
permutacao (h:t)= do x <- permutacao t  
                     insereASorte h x 

-- <- qd a direita esta um programa  =  qd a direita esta uma função
insereASorte :: a -> [a] -> IO [a]
insereASorte x [] = return [x]
insereASorte x l = do p <- randomRIO (0,length l-1)
                   let (a,b) = splitAt p l -- divide a lista em 2 os primeiros p elementos ficam numa e o resto noutra
                    return (a ++ (x:b)) 


--                     linhas palavras caracteres
wordCount :: String -> IO (Int, Int, Int)                    
wordCount  nome = do conteudo  <- readFile nome 
                     let linhas = length (lines conteudo) -- lines divide em varias linhas 
                     let palavras = length (words conteudo)
                     let caracteres = length conteudo
                   