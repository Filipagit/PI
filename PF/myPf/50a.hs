import Data.Char 
import Data.List
import Data.Either
--1 enumFromTo 1 5 corresponde a lista [1,2,3,4,5] conseguii
isenumFromTo :: Int -> Int -> [Int]
isenumFromTo x y = if x<= y
	               then x: isenumFromTo (x+1) y 
	               else []

--2 enumFromThenTo 1 3 10 corresponde `a lista [1,3,5,7,9]  3-1=2   try again
isenumFromThenTo :: Int -> Int -> Int -> [Int]
isenumFromThenTo x y z | y==0 = []
                       | z<y = [x]
                       |otherwise = x : isenumFromThenTo x (x+(y-x)) z

--3 (++) conseguiii
concatenal :: [a] -> [a] -> [a]
concatenal [] l = l
concatenal l [] = l
concatenal (x:xs) l = x:concatenal xs l 

--4 conseguiii
calculael :: [a] -> Int -> a
calculael (x:xs) n = if n==0 
	                 then x 
	                 else calculael xs (n-1)

--5 conseguiiii
isreverse :: [a] -> [a]
isreverse [] = []
isreverse (x:xs) = reverse xs ++ [x]          

--6 conseguiii
istake :: Int -> [a] -> [a]
istake _ []= []
istake 0 l = []
istake n (x:xs)= x:istake (n-1) xs 

--7 conseguiii
isdrop :: Int -> [a] -> [a]
isdrop _ [] = []
isdrop 0 l = l 
isdrop n (x:xs)= isdrop (n-1) xs

--8 conseguii
iszip :: [a] -> [b] -> [(a,b)]
iszip (x:xs) (h:t) = (x,h) : iszip xs t 
iszip _ _ = []

--9 conseguiii
iselem :: Eq a => a -> [a] -> Bool
iselem _ [] = False
iselem n (h:t) =  if n==h 
	              then True
	              else iselem n t 

--10 conseguii
isreplicate :: Int -> a -> [a]
isreplicate 0 _ = []
isreplicate n x = x: isreplicate (n-1) x 

--11 conseguii
isintersperce :: a -> [a] -> [a]
isintersperce _ []= []
isintersperce _ [x] = [x]
isintersperce n (h:t) = h:n: isintersperce n t   

--12 
isgroup :: Eq a => [a] -> [[a]]
isgroup [] = [[]]
isgroup (x:xs) = aux [x] xs 
          where aux x [] = [x]
                aux x (y:ys) = if elem y x then aux (y:x) ys
                                           else x: aux [y] ys

--13 conseguiii
isconcat :: [[a]] -> [a]
isconcat [[]] = []
isconcat (l:ls)= l ++ isconcat ls 

--14 conseguiii
isinits :: [a] -> [[a]]
isinits [] = [[]]
isinits l = isinits(init l) ++ [l]

--15 conseguiii
istails :: [a] -> [[a]]
istails [] = [[]]
istails l = [l] ++ istails(tail l)

--16 conseguiiii
myisPrefixOf :: Eq a => [a] -> [a] -> Bool
myisPrefixOf _ []=False 
myisPrefixOf [] _ = True
myisPrefixOf (x:xs)(h:t)= if x==h 
	                      then myisPrefixOf xs t 
	                      else False 

--17 conseguiii
myisSuffixOf :: Eq a => [a] -> [a] -> Bool
myisSuffixOf [] _ = True
myisSuffixOf _ [] =  False
myisSuffixOf l1 l2 = if last l1== last l2 
	                 then myisSuffixOf(init l1)(init l2)
	                 else False 	

--18 conseguiiiii
myisSubsquenceOf :: Eq a => [a] -> [a] -> Bool
myisSubsquenceOf [] _ =True
myisSubsquenceOf _ [] =False 
myisSubsquenceOf (x:xs) (h:t) = if x==h 
                                then myisSubsquenceOf xs t 
                                else myisSubsquenceOf (x:xs) t            

--19
iselemIndices :: Eq a => a -> [a] -> [Int]
iselemIndices _ [] = []
iselemIndices n l = aux 0 n l 
               where aux _ _ [] = []
                     aux i n (x:xs) = if n==x then i:aux (i+1) n xs 
                                              else aux (i+1) n xs
--20 conseguiiii
isnub :: Eq a => [a] -> [a] 
isnub [] = []
isnub (h:t)= if elem h t
	         then isnub t 
	         else h: isnub t 

--21 conseguiii
isdelete :: Eq a => a -> [a] -> [a]
isdelete _ [] = []
isdelete n (h:t) = if n==h   
                   then t 
                   else h: isdelete n t 

--22 conseguiiii
removepoc :: Eq a => [a] -> [a] -> [a]
removepoc l [] = l 
removepoc [] _ = []
removepoc (x:xs)(h:t)= if x==h 
                       then removepoc xs t 
                       else x: removepoc xs (h:t)             

--23 conseguiii
isunion :: Eq a => [a] -> [a] -> [a]
isunion [] l = l 
isunion l [] = l
isunion l (h:t) = if elem h l 
	              then isunion l t 
	              else  isunion l t ++ [h]

--24 conseguiiii
isintersect :: Eq a => [a] -> [a] -> [a]
isintersect [] l = []
isintersect l [] = []
isintersect (h:t) l = if elem h l 
	                  then h: isintersect t l 
	                  else isintersect t l 

--25 conseguiii      
isinsert :: Ord a => a -> [a] -> [a]
isinsert x [] = [x]
isinsert x (h:t)= if x<h 
	              then x:h:t 
	              else h: isinsert x t 

--26 conseguiii
isunwords :: [String] -> String 
isunwords [] = ""
isunwords (h:t)= h ++ " " ++ isunwords t 

--27 conseguiii
isunlines :: [String] -> String
isunlines [] = "\n" 
isunlines (h:t)= h ++ "\n" ++ isunlines t        

--28 
ispMaior :: Ord a => [a] -> Int 
ispMaior (h:t)= if(maximum(h:t)==h)
	            then 0 
	            else 1+ispMaior t 

--29 conseguiii
istemRepetidos :: Eq a => [a] -> Bool
istemRepetidos [] = False
istemRepetidos (h:t)= if elem h t 
                      then True
                      else istemRepetidos t 	            

--30 conseguiii
isalgarismos :: [Char] -> [Char]
isalgarismos [] = []
isalgarismos (h:t)= if elem h ['0'..'9']
                    then h:isalgarismos t 
                    else isalgarismos t   

--31 
isposImpares :: [a] -> [a]
isposImpares [] = []
isposImpares [x]=[]
isposImpares (h:xs:t) = xs: isposImpares t

--32 conseguiii
isposPares :: [a] -> [a]
isposPares [] = []
isposPares [x]=[x]
isposPares (h:xs:t)= h:isposPares t                                        

--33 conseguiiii
isSorted' :: Ord a => [a] -> Bool 
isSorted' [] = True 
isSorted' [x] = True
isSorted' (h:xs:t)= if h<=xs 
	               then isSorted' (xs:t)
	               else False 

--34 calcula o resultado de ordenar uma lista 
isiSort :: Ord a => [a] -> [a]
isiSort [] = []
isiSort (x:xs) = insert x (isiSort xs)

--35 conseguii
ismenor :: String -> String -> Bool
ismenor [] _ = True 
ismenor _ [] = False
ismenor (x:xs)(h:t)|ord x < ord h = ismenor xs t 
                   |ord x > ord h = False 
                   |otherwise= ismenor xs t

--36 conseguiiii
iselemMSet :: Eq a => a -> [(a,Int)] -> Bool
iselemMSet _ [] = False
iselemMSet e ((x,y):t)= if e==x 
	                    then True 
	                    else iselemMSet e t 

--37 conseguiiii
islengthMSet :: [(a,Int)] -> Int 
islengthMSet [] = 0
islengthMSet ((x,y):t)=y+islengthMSet t 	                    

--38 quasee
isconverteMSet :: [(a,Int)] -> [a]
isconverteMSet [] = []
isconverteMSet ((x,y):t) = aux (x,y) ++ isconverteMSet t
     where aux (_,0) = []
           aux (a,n) = [a] ++ aux (a,n-1) 

--39 conseguiii
isinsereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
isinsereMSet e []=[(e,1)]
isinsereMSet e ((x,y):t)= if e==x 
	                      then (x,y+1):t
	                      else (x,y):isinsereMSet e t 

--40 conseguiii
isremoveMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]	                      
isremoveMSet e [] = []
isremoveMSet e ((x,y):t) |(e== x) && (y>1) =(x,y-1):t 
                         |(e==x) && (y==1) = t 
                         |otherwise = (x,y) : isremoveMSet e t 

--41 
isconstroiMSet :: Ord a => [a] -> [(a,Int)]
isconstroiMSet [] =[]
isconstroiMSet l= aux 1 l 
             where aux i [x] = [(x,i)]
                   aux i (h:xs:t) |h==xs = aux(i+1) (h:t)
                                  |h/=xs= (h,i):aux 1 (xs:t)   
--42
ispartitionEithers :: [Either a b] -> ([a],[b])
ispartitionEithers l = (left l,right l)
              where left (Left a :t)= a:left t 
                    left (right a :t) = left t 
                    left _ =[]
                    right(left a:t)= right t
                    right(right a:t)=a:right t 
                    right _=[]

--43 conseguiiii
iscatMaybes :: [Maybe a] -> [a] 
iscatMaybes []=[]
iscatMaybes (Just a:t)= a : iscatMaybes t 
iscatMaybes (Nothing:t)=iscatMaybes t  

--44 conseguiii
data Movimento = Norte | Sul | Este | Oeste
deriving Show

isposicao :: (Int,Int) -> [Movimento] -> (Int,Int)
isposicao (x,y) [] =(x,y)
isposicao (x,y) (Norte:t) =isposicao(x,y+1)t
isposicao (x,y) (Sul:t)= isposicao(x,y-1)t
isposicao (x,y) (Este:t)= isposicao(x+1,y)t
isposicao (x,y) (Oeste:t)= isposicao(x-1,y)t

--45 conseguii
iscaminho :: (Int,Int) -> (Int,Int) -> [Movimento]
iscaminho (xi,yi) (xf,yf) 
                          |xi==xf && yi==yf = []
                          |xi<xf = [Este]++iscaminho(xi+1,yi)(xf,yf)
                          |xi>xf = [Oeste] ++ iscaminho(xi,yi)(xf+1,yf)
                          |yi<yf = [Norte]++ iscaminho(xi,yi+1) (xf,yf)
                          |yi>yf = [Sul] ++ iscaminho(xi,yi)(xf,yf+1)

--46  conseguiii
isvertical :: [Movimento] -> Bool
isvertical [] =True 
isvertical (Norte:t)= vertical t 
isvertical (Sul:t) = vertical t
isvertical _ = False

--47
data Posicao = Pos Int Int
deriving Show

ismaisCentral :: [Posicao] -> Posicao
ismaisCentral [x] = x 
ismaisCentral (x:y:xs)= if dist x < dist y 
	                    then ismaisCentral (x:xs)
	                    else ismaisCentral (y:xs)
	                   where dist (Pos a b) = sqrt(FromIntegral a^2+b^2)

--48
isvizinhos :: Posicao -> [Posicao] -> [Posicao]
isvizinhos (Pos x y) ((Pos z w):t)= if (y==w && x=z+1 || y==w && x==z-1 || x==z && y==w+1 || x==z && y==w-1)
                                    then Pos z w : isvizinhos ((Pos  x y):t)
                                    else isvizinhos (Pos x y)t


--49 conseguiiii
ismesmaOrdenada :: [Posicao] -> Bool
ismesmaOrdenada [x] =True
ismesmaOrdenada ((Pos x y):(Pos z w):t)= if y==w 
	                                     then mesmaOrdenada(Pos z w):t
	                                     else False 

--50     conseguii                                
data Semaforo = Verde | Amarelo | Vermelho
deriving Show
 
isinterseccaoOk :: [Semaforo] -> Bool
isinterseccaoOk l = contanaov  l <=1
        where contanaov  [] = 0
              contanaov (Vermelho:t)=contanaov t 
              contanaov (_:t) =1+contanaov t 