--1 
--a elemIndices 3 [1,2,3,4,3,2,3,4,5] = [2,4,6].
iselemIndices :: Eq a => a -> [a] -> [Int]
iselemIndices _ [] = []
iselemIndices n l = aux 0 n l 
               where aux _ _ [] = []
                     aux i n (x:xs) = if n==x then i:aux (i+1) n xs 
                                              else aux (i+1) n xs

--b testa se os elementos de uma lista ocorrem noutra pela mesma ordem relativa conseguii
-- isSubsequenceOf [20,40] [10,20,30,40]= True isSubsequenceOf [40,20] [10,20,30,40] = False.
myisSubsequenceOf :: Eq a => [a] -> [a] -> Bool 
myisSubsequenceOf [] _ = True 
myisSubsequenceOf _ []= False 
myisSubsequenceOf (x:xs)(h:t) = if x==h 
	                            then myisSubsequenceOf xs t 
	                            else myisSubsequenceOf (x:xs) t

---2
data BTree a = Empty | Node a (BTree a) (BTree a)

--a que generaliza funcao lookup para arvores binarias de procura conseguiii
lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b 
lookupAP _ Empty = Nothing 
lookupAP n (Node(x,y) e d) |n==x = Just y 
                           |n<x = lookupAP n e
                           |n>x= lookupAP n d 

--b conseguiiii
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e d) (Node y l r) = Node (f x y) (zipWithBT f e l ) (zipWithBT f d r)
zipWithBT f _ _ =Empty

--3 dada uma string, devolve um par de strings: uma apenas com os numeros presentes nessa string, e a outra apenas com as letras presentes
--na string. Implemente a funcao de modo a fazer uma unica travessia da string conseguiii
digitAlpha :: String -> (String,String)
digitAlpha [] = ("","")
digitAlpha (h:t) |isDigit h=(h:a,b)
                 |isAlpha h=(a,h:b)
                 |otherwise=(a,b) 
                 where (a,b)=digitAlpha t 

------4 
data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

--a  recebe uma sequencia nao vazia e devolve o seu primeiro elemento  conseguiiiii               
firstSeq :: Seq a -> a
firstSeq Cons x s = x 
firstSeq (App Nil s) = firstSeq s 
firstSeq (App s _) =firstSeq s  

--b e dropSeq n s elimina os n primeiros elementos da sequencia s
--dropSeq 2 (App (App (Cons 7 (Cons 5 Nil)) (Cons 3 Nil)) (Cons 1 Nil)) ==App (Cons 3 Nil) (Cons 1 Nil)
dropSeq :: Int -> Seq a -> Seq a
dropSeq 0 s = s
dropSeq _ Nil = Nil
dropSeq n (Cons x s) = dropSeq (n-1) s  
dropSeq n (App s1 s2) | n>nx = dropSeq (n-nx) s2
                      | n<nx = App (dropSeq n s1) s2
                      | otherwise = s2 
                     where nx = contaCons s1 

contaCons :: Seq a -> Int
contaCons Nil = 0
contaCons (Cons _ s) = 1 + contaCons s 
contaCons (App s1 s2) = contaCons s1 + contaCons s2 

--c Declare (Seq a) como instˆancia da classe Show de forma a obter o seguinte comportamento no interpretador:
--> App (Cons 1 Nil) (App (Cons 7 (Cons 5 Nil)) (Cons 3 Nil)) <<1,7,5,3>>
instance Show a => Show (Seq a) where
	show x = "<<" ++ mostra x ++ ">>"

mostra :: Show a => Seq a -> String 
mostra 	Nil = ""
mostra (Cons x Nil)= show x
mostra (Cons x s)= show x ++ ";" mostra s 
mostra (App s1 s2) = mostra s1 ++ "," ++ mostra s2 

--5 
type Mat a = [[a]]

--a selecciona aleatoriamente um elemento da matriz
getElem :: Mat a -> IO 
getElem m = do
   let(linhas,colunas) =(length m,length(head m))
                x <- randomRIO(0,linhas-1)
                 y <- randomRIO (0,colunas-1)
                 return ((m !! x) !! y)

--b Um quadrado magico e uma matriz quadrada de inteiros em que a soma de qualquer linha, coluna e das duas diagonais e uma constante.
magic :: Mat Int -> Bool
magic mat = linhasIguaisA n mat && colunasIguaisA n mat && diagonaisIguaisA n mat
    where n = sum (head mat)

linhasIguaisA :: Int -> Mat Int -> Bool
linhasIguaisA n = foldl (\acc l -> sum l == n && acc) True 

colunasIguaisA :: Int -> Mat Int -> Bool
colunasIguaisA n mat = foldl (\acc x -> sum (map (\l -> l !! x) mat) == n && acc) True [0..(length mat - 1)]

diagonaisIguaisA :: Int -> Mat Int -> Bool
diagonaisIguaisA n mat = sum (map (\n -> (mat !! n) !! n) [0..ln]) == n && sum (map (\n -> (mat !! n) !! (ln - n)) [0..ln]) == n
    where ln = length mat - 1
    


