module Teste2019 where

import Data.Char 

---1
--a
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n [] = []
elemIndices n l = aux 0 n l
      where aux _ _ [] = []
            aux i n (x:xs) = if n==x then i: aux (i+1) n xs 
                                     else aux (i+1) n xs

--b
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) = if x==y then isSubsequenceOf xs ys 
                                        else isSubsequenceOf (x:xs) ys 

---2
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

--a
lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP _ Empty = Nothing
lookupAP n (Node (x,y) e d) | n<x = lookupAP n e 
                            | n>x = lookupAP n d
                            | otherwise = Just y 

arvore1 :: BTree (Int,Char) 
arvore1 = Node (5,'e') (Node (3,'c') (Node (2,'b') (Node (1,'a') Empty Empty) Empty) (Node (4,'d') Empty Empty)) (Node (6,'f') Empty Empty) 

--b
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e d) (Node y l r) = Node (f x y) (zipWithBT f e l) (zipWithBT f d r)
zipWithBT f _ _ = Empty

arvore2 :: BTree Int
arvore2 = Node 5 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) (Node 4 Empty Empty)) (Node 6 Empty Empty)

---3
digitAlpha :: String -> (String,String)
digitAlpha [] = ("","")
digitAlpha (x:xs) | isDigit x = (x:a,b)
                  | isAlpha x = (a,x:b)
                  | otherwise = (a,b)
          where (a,b) = digitAlpha xs 

---4
data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

--a
firstSeq :: Seq a -> a
firstSeq (Cons x s) = x 
firstSeq (App Nil s) = firstSeq s 
firstSeq (App s _) = firstSeq s 

--b
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

--c
instance Show a => Show (Seq a) where
    show x = "<<" ++ mostra x ++ ">>"

mostra :: Show a => Seq a -> String
mostra Nil = ""
mostra (Cons a Nil) = show a 
mostra (Cons a s) = show a ++ "," ++ mostra s 
mostra (App s1 s2) = mostra s1 ++ "," ++ mostra s2 

---5
type Mat a = [[a]]

--a
getElem :: Mat a -> IO a
getElem m = do x <- randomRIO (0, length m-1)
               y <- randomRIO (0, length (head m)-1)
               return ((m !! x) !! y)

--b
magic :: Mat Int -> Bool
magic = undefined  

