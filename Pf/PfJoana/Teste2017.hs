module Teste2017 where

---1
type MSet a = [(a,Int)]

--a
cardMSet :: MSet a -> Int
cardMSet [] = 0 
cardMSet ((x,y):xs) = y + cardMSet xs 

--b
moda :: MSet a -> [a]
moda [] = []
moda [(x,y)] = [x]
moda ((a,b):(c,d):t) | b>d = [a]
                     | b==d = a : moda ((c,d):t)
                     | otherwise = []

--c
converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((x,y):xs) = replicate y x ++ converteMSet xs

--d
addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies [] a n = [(a,n)]
addNcopies ((x,y):xs) a n | x==a = (x,y+n):xs
                          | otherwise = insere (x,y) (addNcopies xs a n)
                          where insere (c,d) ((e,f):t) | d>=f = (c,d):(e,f):t
                                                       | otherwise = (e,f):(c,d):t 

---2
data SReais = AA Double Double | FF Double Double
            | AF Double Double | FA Double Double
            | Uniao SReais SReais 

--a
instance Show SReais where
    show (AA x y) = "]" ++ show x ++ "," ++ show y ++ "["
    show (FF x y) = "[" ++ show x ++ "," ++ show y ++ "]"
    show (AF x y) = "]" ++ show x ++ "," ++ show y ++ "]"
    show (FA x y) = "[" ++ show x ++ "," ++ show y ++ "["
    show (Uniao a b) = "(" ++ show a ++ " U " ++ show b ++ ")"

--b
pertence :: Double-> SReais -> Bool
pertence n (AA x y) = n>x && n<y
pertence n (FF x y) = n>=x && n<=y 
pertence n (AF x y) = n>x && n<=y
pertence n (FA x y) = n>=x && n<y
pertence n (Uniao a b) = pertence n a || pertence n b 

--c
tira :: Double -> SReais -> SReais
tira n (AA x y) = if pertence n (AA x y) then Uniao (AA x n) (AA n y) else (AA x y)
tira n (FF x y) | pertence n (FF x y) && n/=x && n/=y = Uniao (FA x n) (AF n y) 
                | pertence n (FF x y) && n==x = (AF x y)
                | pertence n (FF x y) && n==y = (FA x y)
                | otherwise = (FF x y)
tira n (AF x y) | pertence n (FF x y) && n/=y = Uniao (AA x n) (AF n y)
                | pertence n (FF x y) && n==y = (AA x y)
                | otherwise = (AF x y)
tira n (FA x y) | pertence n (FA x y) && n/=x = Uniao (FA x n) (AA n y)
                | pertence n (FA x y) && n==x = (AA x y)
                | otherwise = (FA x y)
tira n (Uniao a b) = if pertence n (Uniao a b) then Uniao (tira n a) (tira n b)
                                               else Uniao a b 

---3
data RTree a = R a [RTree a]

--a
percorre :: [Int] -> RTree a -> Maybe [a]
percorre [] (R x _) = Just [x]
percorre (h:t) (R x l) | h > length l = Nothing
                       | otherwise = case (percorre t (l !! (h-1))) of 
                                     Just c -> Just (x:c)
                                     Nothing -> Nothing

--b
procura :: Eq a => a -> RTree a -> Maybe [Int]
procura x (R y l) | x==y = Just []
procura x (R y []) | x/=y = Nothing
procura x (R y (h:t)) = case (procura x h) of 
                        Just c -> Just (1:c)
                        Nothing -> case (procura x (R y t)) of 
                                   Nothing -> Nothing
                                   Just [] -> Just []
                                   Just (n:ns) -> Just ((n+1):ns)