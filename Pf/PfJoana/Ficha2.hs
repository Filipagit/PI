import Data.Char

---3
--a 
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) = if isDigit x then x:soDigitos xs 
                                else soDigitos xs 
--b
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) = if isLower x then 1+minusculas xs
                                 else minusculas xs 

--c
nums :: String -> [Int]
nums [] = []
nums (x:xs) = if isDigit x then (digitToInt x):nums xs
                           else nums xs 

---4
type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a
conta :: Int -> Polinomio -> Int 
conta n [] = 0
conta n ((c,e):xs) = if e==n then 1+conta n xs
                             else conta n xs 

--b
grau :: Polinomio -> Int
grau [(a,b)] = b
grau ((a,b):t) = if b>grau t then b
                             else grau t 


--c
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau g ((a,b):t) = if g==b then (a,b):selgrau g t
                              else selgrau g t 

--d
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((a,b):t) = if b>0 then ((fromIntegral b)*a,b-1):deriv t
                         else deriv t

--e
calcula :: Float -> Polinomio -> Float 
calcula _ [] = 0
calcula n ((a,b):xs) = a*(n^b) + calcula n xs 

--f
simp :: Polinomio -> Polinomio
simp [] = []
simp ((a,b):xs) = if b==0 then simp xs
                          else (a,b):simp xs   

--g
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (a,b) ((x,y):xs) = (a*x,b+y):mult (a,b) xs

--h
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [(a,b)] = [(a,b)]
normaliza ((a1,b1):(a2,b2):xs) = if b1==b2 then normaliza ((a1+a2,b1): xs)
                                           else if conta b1 xs == 0 then (a1,b1):normaliza ((a2,b2):xs)
                                                                    else normaliza ((a1,b1):xs) ++ [(a2,b2)]  

{--i
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2) 

--j
produto :: Polinomio -> Polinomio -> Polinomio
produto (h:t) p = (mult h p) ++ produto t p 
--}

--k
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((a,b):xs) = insereM (a,b) (ordena xs)
   where insereM (a,b) [] = [(a,b)]
         insereM (a,b) ((a1,b1):xs) = if b<b1 then (a,b):((a1,b1):xs)
                                              else (a1,b1): insereM (a,b) xs  

--l
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)



