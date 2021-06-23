import Data.Char 
import Data.List

---1 
--a retorna a lista resultante de remover da primeira lista os elementos que nao pertencem a segunda. 
 --intersect [1,1,2,3,4] [1,3,5] =[1,1,3] conseguiii
isintersect :: Eq a => [a] -> [a] -> [a]
isintersect _ [] = []
isintersect [] _ = []
isintersect (x:xs) l = if elem x l 
	                 then x: isintersect xs l 
	                 else isintersect xs l 

--b calcula a lista dos sufixos de uma lista 
--tails [1,2,3]=[[1,2,3],[2,3],[3],[]]	conseguiii                
istails :: [a] -> [[a]]
istails [] = [[]]
istails l = [l] ++ istails(tail l)

---2
type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

--a dado um conjunto, da como resultado a lista dos elementos desse conjunto.
--elems [(1,4),(7,8),(19,19),(21,23)]=[1,2,3,4,7,8,19,21,22,23] conseguiii
elems :: ConjInt -> [Int]
elems [] = []
elems ((x,y):t)= [x..y] ++ elems t 

--b que recebe uma lista de inteiros, ordenada por ordem crescente e sem repeticoes, e gera um conjunto. 
--geraconj [1,2,3,4,7,8,19,21,22,23] = [(1,4),(7,8),(19,19),(21,23)] conseguiii
geraconj :: [Int] -> ConjInt 
geraconj [] = []
geraconj l =junta(gera l) 


gera :: [Int] -> ConjInt
gera [] = []
gera [x] = [(x,x)]
gera (h:xs:t)=if xs==h+1 
	               then (h,xs):gera t 
	               else (h,h):gera (xs:t)

junta :: ConjInt -> ConjInt
junta []=[]
junta [(x,y)] =[(x,y)]
junta ((x,y):(z,w):t)= if z==y+1
                       then (x,w):junta t 
                       else (x,y): junta ((z,w):t)               

--------3
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
   deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]                      

--a dado um nome,um email e uma agenda, acrescenta essa informacao a agenda.
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail  nome email agenda = agenda ++ [(nome, [Email email])]

--b dado um nome e uma agenda, retorna a lista dos emails associados a esse nome. Se esse nome nao existir na agenda a
--funcao deve retornar Nothing.
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing
verEmails nome ((x,y):xs) | nome == x = Just (emails y)
                          | otherwise = verEmails nome xs 
                  where emails :: [Contacto] -> [String]
                        emails [] = []
                        emails (Email x:xs) = x : emails xs
                        emails (_:xs) = emails xs 

--c  dada lista de contactos,retorna o par com a lista de numeros de telefone (tanto telefones fixos como telemoveis) e a
--lista de emails, dessa lista. Implemente a funcao de modo a fazer uma unica travessia da lista de contactos.                        
consulta :: [Contacto] -> ([Integer],[String])
consulta [] = ([],[])
consulta (x:xs) | isTelefone x = ((consTelefs x):a,b)
                | isEMail x = (a,(email x):b)
                | otherwise = (a,b)
                where (a,b) = consulta xs 

isTelefone :: Contacto -> Bool
isTelefone c = case c of 
               Casa _ -> True
               Trab _ -> True
               Tlm _ -> True
               _ -> False

consTelefs :: Contacto -> Integer
consTelefs x = case x of 
               Casa n -> n
               Trab n -> n
               Tlm n -> n 

isEMail :: Contacto -> Bool
isEMail e = case e of 
            Email _ -> True
            _ -> False 

email :: Contacto -> String
email x = case x of 
          Email n -> n 

--d dada uma agenda, lê do teclado o nome que pretende consultar e apresenta no ecran os contactos associados a esse nome na agenda.
consultaIO :: Agenda -> IO ()
consultaIO agenda = do putStrLn "Nome que pretende consultar:"
                       s <- getLine
                       n <- consultaNum s agenda
                       putStrLn (show n)  

consultaNum :: Nome -> Agenda -> IO [Contacto]
consultaNum nome [] = return []
consultaNum nome ((x,y):xs) = if nome == x then return y 
                                           else consultaNum nome xs  

--4 
data RTree a = R a [RTree a]  deriving (Show,Eq)

--a dada uma arvore calcula todos os caminhos desde a raiz ate as folhas
--paths (R 1 [R 2 [], R 3 [R 4 [R 5 [], R 6 []]],R 7 []]) = [[1,2],[1,3,4,5],[1,3,4,6],[1,7]]
paths :: RTree a -> [[a]]
paths (R x []) = [[x]]
paths (R n l) = [n : x | x <- concat [paths branch | branch <- l]]        