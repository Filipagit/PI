--1 

--a testa se um predicado é verdade para algum elemento da lista conseguiiii
myany :: (a -> Bool) -> [a] -> Bool
myany f [] =False
myany f (h:t) = if f h 
	          then True 
	          else myany f t

--b combina os elementos de 2 listas usando uma função especifica conseguiii
myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f (x:xs) (h:t) = (f x h) : myzipWith f xs t 
myzipWith f _ _ = []

--c determina  os primeiros elementos da lista que satisfazem o predicado conseguiii
mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile f [] = []
mytakeWhile f (h:t) = if f h 
	                  then h : mytakeWhile f t 
	                  else []

--d elimina os primeiros elementos da lista que satisfazem um dado predicado conseguiiii
mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile f [] = []
mydropWhile f (h:t) = if f h 
                      then mydropWhile f t 
                      else (h:t)	                  

--e conseguuiiiii
myspan :: (a -> Bool) -> [a] -> ([a],[a])
myspan f [] = ([],[])
myspan f (h:t) = if f h 
                 then (h:a,b)
                 else ([],h:t)
                 where (a,b)= myspan f t                      

--f conseguiiii
mydeleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
mydeleteBy f x [] = []
mydeleteBy f x (h:t)= if f x h 
	                  then t 
	                  else h:mydeleteBy f x t 

--g 
mysortOn :: Ord b => (a -> b) -> [a] -> [a]
mysortOn f [] = []
mysortOn f (h:t) = insert' f h (mysortOn f t)
           where insert' :: Ord b => (a -> b) -> a -> [a] -> [a]
                 insert' f x [] = [x]
                 insert f x (h:t) =  if (f x) < (f h) then x:h:t 
                                               else h:insert' f x t

--2 
type Polinomio = [Monomios]                                               
type Monomio = (Float,Int)

--a 
selgrausup :: Int -> Polinomio -> Polinomio
selgrausup  g p = filter(\(n,gr) -> gr==g) p

--b 
contasup :: Int -> Polinomio -> Int   
contasup n p = foldr(\(num,g) s ->if n==g then s+1 else s ) 0 p 

--c 
grausup :: Polinomio -> Int 
grausup ((c,g):p) = foldr (\(_,g2) g1 -> max g2 g1) g p 

--d
derivsup :: Polinomio -> Polinomio
derivsup ((n,g):p) = foldr(\(n,g) -> n*(fromIntegral g))p


--3
type Mat a = [[a]]

--a 
dimOK :: Mat a -> Bool
dimOK [] = True 
dimOK (h:t)= all(\x -> length x == length h ) t

--d
transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = l: transpose rm 
   where l= map head m 
         rm = map tail mf