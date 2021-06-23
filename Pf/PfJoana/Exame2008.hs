module Exame2008 where

----ParteI
---1
data Tree a = Empty | Node a (Tree a) (Tree a)

arv :: Tree Float 
arv = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))

menoresMaiores :: Float -> Tree Float -> ([Float],[Float])
menoresMaiores _ Empty = ([],[])
menoresMaiores n (Node x e d) | x<n = (x:x1++x2,y1++y2)
                              | x>n = (x1++x2,x:y1++y2)
                              | otherwise = (x1++x2,y1++y2)
                            where (x1,y1) = menoresMaiores n e 
                                  (x2,y2) = menoresMaiores n d

---2
ePrefixo :: String -> String -> Bool
ePrefixo [] _ = True
ePrefixo _ [] = False
ePrefixo (x:xs) (y:ys) = if x==y then ePrefixo xs ys
                                 else False

---3
type Extracto = [Movimento]
type Movimento = (Descricao, Tipo, Data, Montante)
type Descricao = String
data Tipo = Credito | Debito deriving Eq
type Ano = Int
type Mes = Int
type Dia = Int
data Data = D Ano Mes Dia
  deriving (Eq,Ord)
type Montante = Float -- valor positivo

--a
procura :: Descricao -> Extracto -> [(Tipo,Data,Montante)]
procura _ [] = []
procura desc ((d,t,dt,mont):xs) = if desc == d then (t,dt,mont):procura desc xs 
                                               else procura desc xs 

--b
{--saldo :: Extracto -> Float
saldo ((desc,t,dt,mont):xs) =  
--}

--c
porOrdem :: Extracto -> Bool
porOrdem [] = True
porOrdem ((_,_,(D a1 m1 d1),_):l@(_,_,(D a2 m2 d2),_):xs) = if a1<a2 || (a1==a2) && (m1<m2) || (a1==a2) && (m1==m2) && (d1<=d2) then porOrdem (l:xs) 
                                                                                                                                else False 

--d
dmaxDebito :: Extracto -> Maybe (Data,Montante)
dmaxDebito ((_,Debito,d,m):t) = maxdeb (d,m) (dmaxDebito t)
dmaxDebito (h:t) = dmaxDebito t
dmaxDebito [] = Nothing

maxdeb :: (Data,Montante) -> Maybe (Data,Montante) -> Maybe (Data,Montante)
maxdeb (d,m) Nothing = Just (d,m)
maxdeb (d,m) (Just (dt,mont)) = if mont>m then Just (dt,mont) else Just (d,m)

----ParteII
---1
--a
--relatorio :: Extracto -> String

--b
aDescoberto :: Extracto -> [(Data,Montante)]
aDescoberto [] = []
aDescoberto ((_,_,d,m):xs) = if m<0 then (d,m):aDescoberto xs 
                                    else aDescoberto xs 

---2
--a
proximo :: Integer -> Integer
proximo x = fromDigits (proximoAux (toDigits x) 1)

proximoAux :: [Integer] -> Integer -> [Integer]
proximoAux [] _ = []
proximoAux [x] i = [i,x]
proximoAux (x:y:xs) i = if x==y then proximoAux (y:xs) (i+1) 
                                else i:x:proximoAux (y:xs) 1 

toDigits :: Integer -> [Integer]
toDigits n | n < 1 = []
           | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

--b
anterior :: Integer -> Integer
anterior x = fromDigits (anteriorAux (toDigits x))

anteriorAux :: [Integer] -> [[Integer]]
anteriorAux [] = []
anteriorAux (x:y:xs) = (replicate (fromInteger x) y) ++ anteriorAux xs  

-- 312211 -> 111221 