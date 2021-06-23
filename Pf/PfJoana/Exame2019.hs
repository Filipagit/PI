module Exame2019 where 

---1
--a
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = if x<y then isSorted (y:xs)
                           else False 

--b
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

---2
maximumMB :: (Ord a) => [Maybe a] -> Maybe a
maximumMB [] = Nothing
maximumMB [Just x] = Just x 
maximumMB (Nothing:xs) = maximumMB xs
maximumMB (Just x: Nothing:xs) = maximumMB (Just x:xs)
maximumMB (Just x:Just y:xs) = if x>y then maximumMB (Just x:xs)
                                      else maximumMB (Just y:xs)

---3
data LTree a = Tip a | Fork (LTree a) (LTree a)

--a
listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d 

--b
instance Show a => Show (LTree a) where
    show (Tip x) = show x ++ "\n"
    show (Fork e d) = mostra 1 e ++ mostra 1 d 

mostra :: Show a => Int -> LTree a -> String
mostra n (Tip x) = replicate n '.' ++ show x ++ "\n"
mostra n (Fork e d) = mostra (n+1) e ++ mostra (n+1) d

---4
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = foldl (\acc x -> max (sum x) acc) (sum l) (inits' l)

---5
type RelP a = [(a,a)]
type RelL a = [(a,[a])]
type RelF a = ([a], a->[a])

--a
convPL :: (Eq a) => RelP a -> RelL a
convPL [] = []
convPL [(x,y)] = [(x,[y])]
convPL (h:t) = junta h (convPL t)
   where junta (a,b) l = if elem a (map fst l) 
                         then map (\(c,d) -> if c==a then (c,b:d) else (c,d)) l 
                         else (a,[b]):l 

--b
{--criaRelPint :: Int -> IO (RelP Int)
criaRelPint 0 = []
criaRelPint n = do 
--}

--c
--i
convFP :: (Eq a) => RelF a -> RelP a



