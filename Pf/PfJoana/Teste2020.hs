module Teste2020 where

---1
--a
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' (x:xs) l = if elem x l then x: intersect' xs l 
                                  else intersect' xs l 
intersect' _ _ = []

--b
tails' :: [a] -> [[a]]
tails' [] = []
tails' (x:xs) = (x:xs) : tails' xs

---2
type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

--a
elems :: ConjInt -> [Int]
elems [] = [] 
elems ((x,y):xs) = if x/=y then enumFromTo x y ++ elems xs 
                           else x: elems xs

--b
geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj l = makeConj (agrupa l)  

agrupa :: [Int] -> [[Int]]
agrupa [] = [[]]
agrupa (a:as) = aux [a] as 
   where aux x [] = [x]
         aux x (y:ys) = if head x == y-1 then aux (y:x) ys 
                                         else [x] ++ aux [y] ys 

makeConj :: [[Int]] -> ConjInt
makeConj [] = []
makeConj ([x]:ys) = (x,x) : makeConj ys
makeConj ((x:xs):ys) = (last xs,x) : makeConj ys

---3
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
     deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]

--a
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email agenda = agenda ++ [(nome, [Email email])]

--b
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing
verEmails nome ((x,y):xs) | nome == x = Just (emails y)
                          | otherwise = verEmails nome xs 
                  where emails :: [Contacto] -> [String]
                        emails [] = []
                        emails (Email x:xs) = x : emails xs
                        emails (_:xs) = emails xs 

--c
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

--d
consultaIO :: Agenda -> IO ()
consultaIO agenda = do putStrLn "Nome que pretende consultar:"
                       s <- getLine
                       n <- consultaNum s agenda
                       putStrLn (show n)  

consultaNum :: Nome -> Agenda -> IO [Contacto]
consultaNum nome [] = return []
consultaNum nome ((x,y):xs) = if nome == x then return y 
                                           else consultaNum nome xs  

agenda1 :: Agenda
agenda1 = [("Joana", [Casa 2530004111, Tlm 935316562, Email "joanaveigaoliveira"]), ("Maria", [Casa 253657842, Trab 919567354])] 

---4
data RTree a = R a [RTree a] deriving (Show, Eq)

--a
paths :: RTree a -> [[a]]
paths (R x []) = [[x]]
paths (R n l) = [n : x | x <- concat [paths branch | branch <- l]]

