module Teste2009 where 

----ParteI
---1
paresImpares :: [Int] -> ([Int],[Int])
paresImpares [] = ([],[])
paresImpares (x:xs) | even x = (x:a,b)
                    | odd x = (a,x:b)
                    | otherwise = (a,b) 
                  where (a,b) = paresImpares xs

---2
sufixo :: (Eq a) => [a] -> [a] -> Bool
sufixo [] _ = True
sufixo _ [] = False
sufixo l1 l2 = prefixo (reverse l1) (reverse l2)

prefixo :: (Eq a) => [a] -> [a] -> Bool
prefixo [] _ = True
prefixo _ [] = False
prefixo (x:xs) (y:ys) = if x==y then prefixo xs ys 
                                else False

---3
data ABin a = Vazia | No a (ABin a) (ABin a)

altura :: ABin a -> Int
altura Vazia = 0 
altura (No x e d) = 1 + max (altura e) (altura d)

---4
type TabPrecos = [(Produto,Preco)] -- preço em euros
type Produto = String
type Preco = Float
type Cotacoes = [(Moeda,Valor)] -- das diversas moedas face ao euro
type Moeda = String
type Valor = Float

cot :: Cotacoes
cot = [("Dolar",1.1),("Iene",121.5),("Libra",0.85),("Real",4.6)]

tabprc :: TabPrecos
tabprc = [("Banana",1.2),("Pera",0.9),("Kiwi",1.5),("Iogurte",2.0)]

--a
maiorValor :: Cotacoes -> (Moeda,Valor)
maiorValor [(m,v)] = (m,v)
maiorValor ((m1,v1):(m2,v2):xs) = if v1>v2 then maiorValor ((m1,v1):xs)
                                           else maiorValor ((m2,v2):xs)

--b
convPrecos :: TabPrecos -> Cotacoes -> Moeda -> TabPrecos
convPrecos ((prod,prc):xs) ((m,v):ys) moeda = if moeda==m then (prod,prc*v):convPrecos xs [(m,v)] moeda
                                                          else convPrecos ((prod,prc):xs) ys moeda 
convPrecos _ _ _ = []

--c
precosMultiMoeda :: TabPrecos -> [Moeda] -> Cotacoes -> [(Produto,[(Moeda,Preco)])]
precosMultiMoeda t m c = map fun t
                     where fun (n,p) = (n, aux p m c)

aux :: Preco -> [Moeda] -> Cotacoes -> [(Moeda,Preco)]
aux prc (x:xs) c = if elem x (map fst c) then aux2 prc x c : aux prc xs c
                                         else aux prc xs c   
aux _ _ _ = []

aux2 :: Preco -> Moeda -> Cotacoes -> (Moeda,Preco)
aux2 prc moeda ((m,v):xs) = if moeda==m then (m,prc*v)
                                        else aux2 prc moeda xs 

----ParteII
type Horario = [(Dia,[(Hora,Hora,Actividade)])]
data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
   deriving (Eq, Ord)
type Hora = (Int,Int) -- horas, minutos
data Actividade = Aula Disciplina Tipo Sala
                | Outra String
           deriving Eq
type Disciplina = String
data Tipo = T | TP | PL deriving Eq
type Sala = String

---1
consulta :: Dia -> Hora -> Horario -> Maybe Actividade
consulta _ _ [] = Nothing
consulta dia hora ((d,((h1,h2,atv):xs)):ys) | dia==d && hora>=h1 && hora<=h2 = Just atv 
                                            | dia==d && (hora<h1 || hora>h2) = consulta dia hora ((d,xs):ys)
                                            | dia/=d = consulta dia hora ys 

---2
aulasPorDia :: Horario -> [(Dia,Hora)]
aulasPorDia [] = []
aulasPorDia ((d,xs):ys) = (d,paraHoras (contaMinutos xs)) : aulasPorDia ys 

contaMinutos :: [(Hora,Hora,Actividade)] -> Int  
contaMinutos ((h1,h2,_):xs) = ((paraMinutos h2) - (paraMinutos h1)) + contaMinutos xs 

paraMinutos :: Hora -> Int 
paraMinutos (h,m) = h*60 + m 

paraHoras :: Int -> Hora 
paraHoras m = (div m 60, mod m 60)

