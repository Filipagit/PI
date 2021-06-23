module Ficha8 where

import Data.List
import Data.Char

---1
data Frac = F Integer Integer

--a
normaliza :: Frac -> Frac
normaliza (F 0 d) = F 0 1 
normaliza (F n d) = F (sinal * (div (abs n) md)) (div (abs d) md)
                 where md = mdc (abs n) (abs d)
                       sinal = if (n*d) > 0 then 1
                                            else (-1)

mdc :: Integer -> Integer -> Integer
mdc x y | x==y = x
        | x>y = mdc (x-y) y 
        | x<y = mdc x (x-y)

--b
instance Eq Frac where 
    (F a b) == (F c d) = a * d == c * b

--c
instance Ord Frac where
    (F a b) <= (F c d) = a * d <= c * b 

--d
instance Show Frac where
    show (F a b) = show a ++ "/" ++ show b

--e
instance Num Frac where
 (+) (F n1 d1) (F n2 d2) = normaliza (F ((n1*d2) + (n2*d1)) (d1*d2))
 (-) (F n1 d1) (F n2 d2) = normaliza (F ((n1*d2) - (n2*d1)) (d1*d2))
 (*) (F n1 d1) (F n2 d2) = normaliza (F (n1*n2) (d1*d2)) 
 abs (F n d) = normaliza (F (abs n) (abs d))
 negate (F n d) = normaliza (F (-n) d)
 signum (F n d) | n==0 = F 0 1
                | n*d < 0 = F (-1) 1
                | n*d > 0 = F 1 1
 fromInteger n = F n 1 

--f
g :: Frac -> [Frac] -> [Frac]
g f l = filter (\f1 -> f1 > 2 * f) l 

---2
data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

--a
instance Show a => Show (Exp a) where
  show (Const a) = show a
  show (Simetrico a) = "(-" ++ show a ++ ")"
  show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Menos a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
  show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

---3
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

--a
instance Ord Data where 
  compare (D d1 m1 a1) (D d2 m2 a2) | a1==a2 && m1==m2 && d1==d2 = EQ 
                                    | a1>a2 || a1==a2 && (m1>m2 || m1==m2 && d1>d2)= GT
                                    | otherwise = LT 

--b
instance Show Data where 
 show (D d m a) = show a ++ "/" ++ show m ++ "/" ++ show d 

--c
ordena :: Extracto -> Extracto
ordena (Ext n l) = (Ext n (sortBy (\(data1,_,_) (data2,_,_) -> compare data1 data2) l))

--d
instance Show Extracto where
	show (Ext st list) = "Saldo anterior: " ++ (show st) ++ "\n----------------------------------------------\nData\t\tDescricao\tCredito\tDebito\n----------------------------------------------\n" ++ (concat (map auxMovimento list)) ++ "----------------------------------------------\nSaldo actual: " ++ (show (saldo (Ext st list))) where
		auxMovimento (dt,strg,Credito x) = if (length strg) < 7 then (show dt) ++ "\t" ++ strg ++ "\t\t" ++ (show x) ++ "\n" else (show dt) ++ "\t" ++ strg ++ "\t" ++ (show x) ++ "\n"
		auxMovimento (dt,strg,Debito x) = if (length strg) < 7 then (show dt) ++ "\t" ++ strg ++ "\t\t\t" ++ (show x) ++ "\n" else (show dt) ++ "\t" ++ strg ++ "\t\t" ++ (show x) ++ "\n"

saldo :: Extracto -> Float
saldo (Ext x lm) = foldl (\acc (_,_,mov) -> case mov of Credito n -> (acc + n)
                                                        Debito n -> (acc - n)) x lm