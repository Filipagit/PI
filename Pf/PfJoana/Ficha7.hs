module Ficha7 where

---1 
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

--a
calcula :: ExpInt -> Int
calcula (Const a) = a 
calcula (Simetrico a) = - (calcula a)
calcula (Mais a b) = (calcula a) + (calcula b)
calcula (Menos a b) = (calcula a) - (calcula b)
calcula (Mult a b) = (calcula a) * (calcula b)

--b
infixa :: ExpInt -> String
infixa (Const a) = show a 
infixa (Simetrico a) = "(-" ++ infixa a ++ ")"
infixa (Mais a b) = "(" ++ infixa a ++ "+" ++ infixa b ++ ")"
infixa (Menos a b) = "(" ++ infixa a ++ "-" ++ infixa b ++ ")"
infixa (Mult a b) = "(" ++ infixa a ++ "*" ++ infixa b ++ ")"

--c
posfixa :: ExpInt -> String
posfixa (Const a) = show a 
posfixa (Simetrico a) = posfixa a ++ " -"
posfixa (Mais a b) = posfixa a ++ " " ++ posfixa b ++ " +"
posfixa (Menos a b) = posfixa a ++ " " ++ posfixa b ++ " -"
posfixa (Mult a b) = posfixa a ++ " " ++ posfixa b ++ " *"

---2
data RTree a = R a [RTree a]

--a
soma :: Num a => RTree a -> a
soma (R x l) = x + sum (map soma l)

--b
altura :: RTree a -> Int
altura (R x []) = 1
altura (R x l) = 1 + maximum (map altura l)

--c
prune :: Int -> RTree a -> RTree a
prune 1 (R x l) = R x []
prune n (R x l) = R x (map (prune (n-1)) l) 

--d 
mirror :: RTree a -> RTree a
mirror (R x []) = R x []
mirror (R x l) = R x (reverse (map mirror l))

--e
postorder :: RTree a -> [a]
postorder (R x l) = concat (map postorder l) ++ [x]

---3
data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)

--a
ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork e d) = ltSum e + ltSum d 

--b
listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d

--c
ltHeight :: LTree a -> Int
ltHeight (Tip x) = 1 
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)

---4
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

--a
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf a) = (Empty,Tip a)
splitFTree (No a e d) = (Node a e1 d1, Fork e2 d2)
           where (e1,e2) = splitFTree e
                 (d1,d2) = splitFTree d

--b
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip a) = Just (Leaf a)
joinTrees (Node x e d) (Fork l r) = Just (No x e1 d1)
           where Just e1 = joinTrees e l 
                 Just d1 = joinTrees d r
joinTrees _ _ = Nothing

arvore1 :: RTree Int 
arvore1 = R 5 [ R 4 [ R 3 [R 17 []], R 2 [], R 7 []], R 10 [], R 1 [ R 8 [ R 0 [], R 20 [], R 15 [], R 39 []], R 12 []]]   

arvore2 :: LTree Int 
arvore2 = Fork (Fork (Tip 2) (Tip 3)) (Fork (Fork (Tip 4) (Tip 5)) (Tip 6))  