import Data.Char (ord)
import Data.List (partition)

--2
--2a dada uma lista produz a lista em que cada elemento é o dobro conseguiii
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t)= 2*h : dobros t 

--2b calcula o nr de vezes que um caracter ocorre numa string  conseguiiii
numOcorre :: Char -> String -> Int 
numOcorre c [] = 0 
numOcorre c (h:t) = if(c==h)
	                then 1 +  numOcorre c t 
	                else  numOcorre c t

--2c testa se uma lista so tem elementos positivos conseguiii
positivos :: [Int] -> Bool
positivos [] = True 
positivos (h:t)= if(h>=0)
                 then positivos t 
                 else False 	
--2d retira todos os elementos nao positivos de uma lista de inteiros conseguiii
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t)= if(h>0)
	         then (h:soPos t)
	         else soPos t 

--2e soma todos os nrs negativos da lista conseguiiiii
somaNeg :: [Int] -> Int 
somaNeg [] = 0
somaNeg (h:t)= if(h<0)
               then h + somaNeg t 
               else somaNeg t 

--2f devolve os 3 ults elementos da lista conseguiiii
tresUlt :: [a] -> [a]
tresUlt []= []
tresUlt (h:t)= if((length(h:t))<=3)
	           then (h:t)
	           else tresUlt t 

--2g calcula a lista das 2 componentes dos pares conseguiii
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):t)= y : segundos t

--2h testa se um elemento aparece como 1 componente de algum dos pares conseguiii
nosPrimeiros :: Eq a => a -> [(a,b)] -> Bool 
nosPrimeiros _ [] = False          
nosPrimeiros n ((x,y):t)= if(n==x)
	                      then True 
	                      else nosPrimeiros n t

--2i soma uma lista de triplos componente a componente 
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t)= let (d,e,f)= sumTriplos t 
                        in (x+d,y+e,z+f)

--3a seleciona os que  sao algarismos 
soDigitos :: [Char] -> [Char]  
soDigitos [] = []
soDigitos (h:t) | elem h ['0'..'9'] = h:soDigitos t 
                |otherwise= soDigitos t  

--3b conta quantos sao minusculas conseguii
minusculas :: [Char] -> Int       
minusculas [] = 0
minusculas (h:t) = if(elem h ['a'..'z'])
                   then 1 +  minusculas t 
                   else minusculas t                              	                      

--3c
nums :: [Char] -> [Int]
nums [] = []
nums (c:str) = if c `elem` ['0'..'9'] then (ord c - ord '0') : nums str 
                                      else nums str


-----4
--[(2,3), (3,4), (5,3), (4,5)] representa o polinomio 2x^3+3x^4+5x^3+4x^5
type Polinomio = [Monomio]
type Monomio = (Float,Int)

--4a conseguiiii
conta :: Int -> Polinomio -> Int 
conta _ [] = 0
conta n ((nu,g):t)= if(n==g)
	                then 1 + conta n t 
	                else conta n t 
                
--4b indica o grau de um polinomio
grau :: Polinomio -> Int 
grau [(nu,g)]=g
grau ((nu,g):t)= if g > grau t 
                 then g 
                 else  grau t 

--4c seleciona os monomios c um determindado grau conseguiiii
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau  grau ((n,g):t)= if grau == g 
	                     then (n,g): selgrau grau t 
	                     else selgrau grau  t 

--4d calcula a derivada de um polinomio  conseguiii
deriv :: Polinomio -> Polinomio
deriv [] =  []
deriv ((n,g):t)= (n*(fromIntegral g),g-1) : deriv t 	--fromIntegral é pra converter o int em float

--4e calcula o valor de um polinomio para um dado valor de x conseguiii
calcula :: Float -> Polinomio -> Float                     
calcula _ [] = 0
calcula x ((n,g):t)= n*(x^g) + calcula x t 

--4f retira os monomios de coef 0 conseguiiii
simp :: Polinomio -> Polinomio 
simp [] = []
simp ((n,g):t) = if n==0
	             then simp t 
	             else (n,g): simp t

--4g calcula o resultado da mult de um mon por pol conseguiiii
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (n1,g1) ((n2,g2):t)= (n1*n2,g1+g2) : mult (n1,g1) t

--4h 
normaliza :: Polinomio -> Polinomio 
normaliza [] = []
normaliza [(n,g)]= [(n,g)]
normaliza ((n1,g1):(n2,g2):t) | g1==g2 = normaliza ((n1+n2,g1) :t)
                              | conta g1 t == 0 = (n1,g1): normaliza((n2,g2):t)
                              |otherwise= normaliza ((n1,g1):t ++ [(n2,g2)])

--4i soma 2 pol normalizados 
soma :: Polinomio -> Polinomio -> Polinomio
soma p [] = p
soma [] p = p
soma p1 p2 = normaliza (p1++p2)

--4j
produto :: Polinomio -> Polinomio -> Polinomio
produto (h:t) p = (mult h p) ++ produto t p                            

--4k ordena o polinomio por ordem crescente de graus 
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((a,b):xs) = insereM (a,b) (ordena xs)
   where insereM (a,b) [] = [(a,b)]
         insereM (a,b) ((a1,b1):xs) = if b<b1 then (a,b):((a1,b1):xs)
                                              else (a1,b1): insereM (a,b) xs  


--4l testa se 2 pol sao eq conseguiii
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena(normaliza p1) == ordena(normaliza p2)


