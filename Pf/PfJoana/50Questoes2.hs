import Data.List
import Data.Char 
import Data.Either

--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y = if x<y then x:enumFromTo (x+1) y 
                         else []

--2 
enumFromThenTo' :: Int -> Int -> Int -> [Int] 
enumFromThenTo' x a y = | x < y && x < a then x:enumFromThenTo' a (a+(a-x)) y    
                        | x==y = [x]
                        | x==a = repeat a 
                        | otherwise = [] 

--3
(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) (x:xs) l = x:(xs +++ l)      

--4
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) n = if n==0 then x
                         else (!!!) xs (n-1)

--5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse xs) ++ [x]

--6
take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs) = if n>0 then x:take' (n-1) xs 
                        else []

--7
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs) = if n>0 then drop' (n-1) xs 
                        else (x:xs) 

--8
zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys 

--9
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False 
elem' n (x:xs) = if n==x then True
                         else elem' n xs 

--10
replicate' :: Int -> a -> [a]
replicate' n a = if n>0 then a:replicate' (n-1) a 
                        else []

--11
intersperse' :: a -> [a] -> [a]
intersperse' n [] = []
intersperse' n [x] = [x]
intersperse' n (x:xs) = x:n:intersperse' n xs 

--12
group' :: Eq a => [a] -> [[a]]
group' [] = [[]]
group' (a:as) = aux [a] as
  where aux x [] = [x]
        aux x (y:ys) = if elem y x then aux (y:x) ys
                                   else x: aux [y] ys

--13 
concat' :: [[a]] -> [a] 
concat' [] = []
concat' (l:ls) = l ++ concat' ls

--14
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l] 

--15
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' (x:xs) = (x:xs): tails' xs 

--16
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = if x==y then isPrefixOf' xs ys 
                                    else False 

--17
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' l1 l2 = if last l1 == last l2 then isSuffixOf' (init l1) (init l2)
                                          else False

--18
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (x:xs) (y:ys) = if x==y then isSubsequenceOf' xs ys
                                         else isSubsequenceOf' (x:xs) ys 

--19
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' n l = aux 0 n l 
   where aux _ _ [] = []
         aux i a (x:xs) = if a==x then i:aux(i+1) a xs
                                  else aux(i+1) a xs 

--20
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' a = aux [] a 
   where aux l [] = l
         aux l (x:xs) = if elem x l then aux l xs
                                    else aux (l++[x]) xs

--21
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (x:xs) = if n==x then xs
                           else x:delete' n xs 

--22
(\\\) :: Eq a => [a] -> [a] -> [a] 
(\\\) [] _ = [] 
(\\\) l [] = l
(\\\) l (x:xs) = (\\\) (delete' x l) xs 

--23
union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l 
union' l (x:xs) = if elem x l then union' l xs
                              else union' (l++[x]) xs 

--24
intersect' :: Eq a => [a] -> [a] -> [a]                                
intersect' (x:xs) l = if elem x l then x:intersect' l xs 
                                  else intersect' l xs 
intersect' _ _ = []

--25
insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (x:xs) = if n<x then n:x:xs
                          else x:insert' n xs 

--26
unwords' :: [String] -> String
unwords' [] = " "
unwords' [x] = x
unwords' (x:xs) = x ++ " " ++ unwords' xs

--27
unlines' :: [String] -> String
unlines' [] = []
unlines' (x:xs) = x ++ "\n" ++ unlines' xs 

--28
pMaior :: Ord a => [a] -> Int
pMaior (h:t) = if (maximum (h:t) /= h) then 1 + pMaior t 
                                       else 0 

--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (x:xs) = if elem x xs then True
                                   else temRepetidos xs 

--30
algarismos :: [Char] -> [Char] 
algarismos [] = []
algarismos (x:xs) = if elem x ['0'..'9'] then x:algarismos xs 
                                         else algarismos xs 

--31
posImpares :: [a] -> [a] 
posImpares l = aux 0 l 
  where aux _ [] = []
        aux n (x:xs) = if odd n then x:aux (n+1) xs 
                                else aux (n+1) xs 

--32
posPares :: [a] -> [a]  
posPares l = aux 0 l 
   where aux _ [] = []
         aux n (x:xs) = if even n then x:aux (n+1) xs 
                                  else aux (n+1) xs 

--33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = if x<y then isSorted xs 
                            else False   

--34
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert' x (iSort xs)

--35
menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = False
menor (x:xs) (y:ys) | ord x < ord y = True 
                    | ord x > ord y = False
                    | otherwise = menor xs ys 

--36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet n ((a,b):xs) = if n==a then True
                                else elemMSet n xs 

--37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0 
lengthMSet ((a,b):xs) = b + lengthMSet xs 

--38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):xs) = replicate' b a ++ converteMSet xs 

--39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet n [] = [(n,1)]
insereMSet n ((a,b):xs) = if n==a then (a,b+1):xs
                                  else (a,b):insereMSet n xs                                    

--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
removeMSet n [] = []
removeMSet n ((a,b):xs) = if n==a && b>1 then (a,b-1):xs
                                         else if n==a && b==1 then xs 
                                                              else (a,b):removeMSet n xs

--41
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet l = aux 1 l 
  where aux i [x] = [(x,i)]
        aux i (x:y:xs) | x==y = aux (i+1) (x:xs)
                       | x/=y = (x,i): aux 1 (y:xs)

--42
partitionEithers' :: [Either a b] -> ([a],[b])  
partitionEithers' l = (left l, right l)
  where left (Left x:xs) = x:left xs 
        left (Right x:xs) = left xs 
        left _ = []
        right (Left x:xs) = right xs
        right (Right x:xs) = x:right xs 
        right _ = []  

--43
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = [] 
catMaybes' (Nothing:xs) = catMaybes' xs 
catMaybes' (Just x:xs) = x:catMaybes' xs 

--44
data Movimento = Norte | Sul | Este | Oeste
               deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao x [] = x 
posicao (a,b) (x:xs) = case x of 
                       Norte -> posicao (a,b+1) xs
                       Sul -> posicao (a,b-1) xs 
                       Este -> posicao (a+1,b) xs 
                       Oeste -> posicao (a-1,b) xs 

--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) | x1==x2 && y1==y2 = []
                        | y1<y2 = Norte:caminho (x1,y1+1) (x2,y2)
                        | y1>y2 = Sul:caminho (x1,y1-1) (x2,y2)
                        | x1<x2 = Este:caminho (x1+1,y1) (x2,y2)
                        | x1>x2 = Oeste:caminho (x1-1,y1) (x2,y2)

--46
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (x:xs) = case x of 
                  Norte -> vertical xs 
                  Sul -> vertical xs 
                  _ -> False                   

--47
data Posicao = Pos Int Int
             deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [x] = x
maisCentral (x:y:xs) = if dist x < dist y then maisCentral (x:xs)
                                          else maisCentral (y:xs)
     where dist (Pos a b) = sqrt (fromIntegral(a^2+b^2))

--48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos a b) ((Pos x y):xs) = if (a==x && b==y+1) || (a==x && b==y-1) || (a==x+1 && b==y) || (a==x-1 && b==y) 
                                    then (Pos x y):vizinhos (Pos a b) xs
                                    else vizinhos (Pos a b) xs 

--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [x] = True
mesmaOrdenada ((Pos x1 y1):(Pos x2 y2):xs) = if y1==y2 then mesmaOrdenada xs 
                                                       else False  

--50
data Semaforo = Verde | Amarelo | Vermelho
              deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l = aux l <= 1 
  where aux [] = 0
        aux (x:xs) = case x of 
                     Vermelho -> 0 + aux xs
                     _ -> 1 + aux xs 