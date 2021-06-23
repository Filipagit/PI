--1a calcula o perimetro de uma circunferencia dado o comprimento do seu raio conseguiiiii
perimetro :: Float -> Float
perimetro r= 2*pi*r

--1b calcula a distancia entre dois pontos no plano cartesiano conseguiiii 
dist :: (Float,Float) -> (Float,Float) -> Float
dist (a,b)(x,y)= sqrt((x-a)^2 +(y-b)^2) 

--1c devolve um par c o primeiro e o ult elemento dessa lista conseguiiii
primUlt :: [a] -> (a,a)
primUlt l = (head l,last l)

--1d testa se m é multiplo de n conseguii
multiplo :: Int -> Int -> Bool
multiplo m n = if mod m n /=0 then False else True 

--1e conseguiii
--truncaImpar :: [a] -> [a]
--truncaImpar l = if (mod length l 2 ==0)
	            --then l 
	            --else tail l 

--1f conseguiii
max2 :: Int -> Int -> Int 
max2 a b = if a>b 
	       then a 
	       else b

--1g conseguiiii
--max3 :: Int -> Int -> Int 
--max3 a b c = if(max2 a b)== a 
            -- then max2 a c 
             --else max2 b c 	       

--2a 
--nRaizes :: a -> a -> a -> Int 
--nRaizes a b c = if(sqrt(b^2-4ac)<0)
     --           then 0 
       --         else if (sqrt(b^2-4ac)==0)
         --            then 1 
           --          else 2

--3 
type Hora = (Int, Int)                                

--3a  conseguiii
horavalida :: Hora -> Bool
horavalida (h,m) = if(h>=0 && h<24 && m>=0 && m<60)
	               then True
	               else False 

--3b conseguiii
testahora :: Hora -> Hora -> Bool 
testahora (h1,m1) (h2,m2)= if((h1==h2 && m1<m2) || (h1<h2))
                           then True 
                           else False 	               

--3c conseguiiii
converteHoras :: Hora -> Int 
converteHoras (h,m)= (h*60)+ m                            

--3d 
convertemin :: Int -> Hora
convertemin m = (div m 60,mod m 60)

--3e
calculadif :: Hora -> Hora -> Int 
calculadif a b   = if(testahora a b)
	               then (converteHoras b) - (converteHoras a)
	               else (converteHoras a) - (converteHoras b)

--3f
addmin :: Int -> Hora -> Hora
addmin n (h,m) 
  | converteHoras(h,m) + n <1440 = convertemin(converteHoras(h,m)+n)
  | converteHoras(h,m) + n >=1440= convertemin(converteHoras(h,m)+n - 1440)
--5--
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)
--5a-- calcula o proximo estado de um semaforo conseguiii
next :: Semaforo -> Semaforo
next s= if(s==Verde)
	    then Amarelo 
	    else if(s==Amarelo)
	    	 then Vermelho
	    	 else Verde
--5b determina se é obrigatorio parar num semaforo conseguiii
stop :: Semaforo -> Bool
stop s = if(s==Vermelho)
	     then True
	     else False 

--5c testa se o estado de 2 semaforos num cruzamento é seguro conseguiii
safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = if(s1==Vermelho || s2==Vermelho)
	         then True 
	         else False 

--6 etc




