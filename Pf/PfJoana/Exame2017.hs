module Exame2017 where 

---1
--a
unlines' :: [String] -> String
unlines' [] = " "
unlines' [x] = x ++ "\n"
unlines' (x:xs) = x ++ "\n" ++ unlines xs

--b
(\\\) :: (Eq a) => [a] -> [a] -> [a]
(\\\) [] _ = []
(\\\) l [] = l 
(\\\) l (x:xs) = (\\\) (delete' x l) xs

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (x:xs) = if n==x then xs 
                           else x:delete' n xs 

---2
data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a

--a
primeiro :: Seq a -> a
primeiro (Inicio x s) = x 
primeiro (Fim Nil x) = x 
primeiro (Fim s x) = primeiro s

--b
semUltimo :: Seq a -> Seq a
semUltimo (Inicio x Nil) = Nil 
semUltimo (Inicio x s) = Inicio x (semUltimo s) 
semUltimo (Fim s x) = s  

---3
data BTree a = Empty | Node a (BTree a) (BTree a)

--a
prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune _ Empty = Empty 
prune n (Node x e d) = Node x (prune (n-1) e) (prune (n-1) d)

--b
semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo (Node x Empty d) = d 
semMinimo (Node x e d) = Node x (semMinimo e) d

---4
type Tabuleiro = [String]

exemplo :: Tabuleiro
exemplo = ["..R.",
           "R...",
           "...R",
           ".R.."]

--a
posicoes :: Tabuleiro -> [(Int,Int)]
posicoes tab = posicoesAux tab (0,0) 

posicoesAux :: Tabuleiro -> (Int,Int) -> [(Int,Int)]
posicoesAux [] _ = [] 
posicoesAux ((x:xs):ys) (a,b) = case x of 
                                '.' -> posicoesAux (xs:ys) (a+1,b) 
                                'R' -> (a,b) : posicoesAux ys (0,b+1)

--b
valido :: Tabuleiro -> Bool
valido tab = foldl (\acc (x,y) -> if length (filter (\(a,b) -> (a,b) /= (x,y) && (a == x || b == y || a - b == x - y || b - a == y - x)) (posicoes tab)) > 0 then False else acc) True (posicoes tab)    



