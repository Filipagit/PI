import Data.Char 

--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y = if x<y then x:enumFromTo (x+1) y 
                         else []

--2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z | x<y && x<z = x:enumFromThenTo' x (x+(y-x)) z
                      | x>y && x<z = []

--3
(+++) :: [a] -> [a] -> [a]
(+++) l [] = l 
(+++) [] l = l 
(+++) (x:xs) l = x:(+++) xs l

--4
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) i = if i==0 then x else (!!!) xs (i-1)

--5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

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
zip' (x:xs) (y:ys) = (x,y):zip' xs ys
zip' _ _ = []

--9
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) = n==x || elem' n xs 

--10
replicate' :: Int -> a -> [a]
replicate' n a = if n>0 then a: replicate' (n-1) a
                        else [] 

--11
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' n (x:xs) = x:n:intersperse' n xs 

--12
group :: Eq a => [a] -> [[a]]
group [] = [[]]
group (x:xs) = aux [x] xs 
          where aux x [] = [x]
                aux x (y:ys) = if elem y x then aux (y:x) ys
                                           else x: aux [y] ys

--13
concat' :: [[a]] -> [a]
concat' [[]] = []
concat' (l:ls) = l ++ concat' ls 

--14
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

--15
tails' :: [a] -> [[a]] 
tails' [] = [[]]
tails' (x:xs) = (x:xs) : tails' xs 

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
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices n l = aux 0 n l 
               where aux _ _ [] = []
                     aux i n (x:xs) = if n==x then i:aux (i+1) n xs 
                                              else aux (i+1) n xs

--20
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = if elem x xs then nub' xs else x:nub' xs 

--21
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (x:xs) = if n==x then xs else x:delete' n xs 

--22
(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) [] _ = []
(\\\) l [] = l 
(\\\) l (x:xs) = (\\\) (delete' x l) xs

--23
union' :: Eq a => [a] -> [a] -> [a]
union' [] l = l 
union' l [] = l
union' l (x:xs) = if elem x l then union' l xs 
                              else union' (l++[x]) xs 

--24
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' (x:xs) l = if elem x l then x: intersect' xs l 
                                  else intersect' xs l 
intersect' _ _ = []

--25
insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (x:xs) = if n<x then n:x:xs
                          else x: insert' n xs 

--26
unwords' :: [String] -> String
unwords' [] = ""
unwords' [x] = x 
unwords' (x:xs) = x ++ " " ++ unwords' xs 

--27
unlines' :: [String] -> String
unlines' [] = "\n"
unlines' [x] = x ++ "\n"
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

--28
pMaior :: Ord a => [a] -> Int
pMaior (h:t) = if (maximum (h:t)/=h) then 1 + pMaior t 
                                     else 0

--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (x:xs) = elem x xs || temRepetidos xs

--30
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (x:xs) = if elem x ['0'..'9'] then x:algarismos xs 
                                         else algarismos xs 

--31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (x:y:xs) = y:posImpares xs 

--32
posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x] 
posPares (x:y:xs) = x:posPares xs 

--33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True 
isSorted (x:y:xs) = if x<y then isSorted (y:xs)
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
