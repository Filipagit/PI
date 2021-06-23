module Teste2016 where

---1
--a
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : filter (/=x) (nub' xs)

--b
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys
zipWith' f _ _ = []

---2
type MSet a = [(a,Int)]

--a
converte :: Eq a => [a] -> MSet a 
converte l = aux 1 l 
       where aux i [x] = [(x,i)]
             aux i (x:y:xs) | x==y = aux (i+1) (x:xs)
                            | x/=y = (x,i) : aux 1 (y:xs)

--b
intersect' :: Eq a => MSet a -> MSet a -> MSet a 
intersect' ((a,b):xs) l | elem a (map fst l) = (a,(min b b2)):intersect' xs l 
                        | otherwise = intersect' xs l
                        where b2 = encontraSec a l   
intersect' _ _ = [] 

encontraSec :: Eq a => a -> MSet a -> Int
encontraSec n ((a,b):xs) = if n==a then b else encontraSec n xs 

---3
data Prop = Var String | Not Prop | And Prop Prop | Or Prop Prop

p1 :: Prop
p1 = Not (Or (And (Not (Var "A")) (Var "B")) (Var "C"))

--a
{--instance Show Prop where
    show (Var x) = x 
    show (Not x) = "-" ++ show x 
    show (And x y) = "(" ++ show x ++ "/\\" ++ show y ++ ")"
    show (Or x y) = "(" ++ show x ++ "\/" ++ show y ++ ")"
--}
--b
eval :: [(String,Bool)] -> Prop -> Bool
eval l (Not x) = not (eval l x)
eval l (Var x) | elem x (map fst l) = encontraSeg x l 
               | otherwise = error ("Valor da variável não encontrado")
eval l (And x y) = (eval l x) && (eval l y)
eval l (Or x y) = eval l x || eval l y  

encontraSeg :: String -> [(String,Bool)] -> Bool
encontraSeg s ((a,b):xs) = if s==a then b else encontraSeg s xs 

--c
nnf :: Prop -> Prop
nnf (Not (Not x)) = x 
nnf (Not (And x y)) = (Or (nnf (Not x)) (nnf (Not y)))
nnf (Not (Or x y)) = (And (nnf (Not x)) (nnf (Not y)))
nnf p = p

--d
avalia :: Prop -> IO Bool 
avalia p = do varsValue <- getValues vars
              where vars = getVars p 

getVars :: Prop -> [String]
getVars (Var x) = [x]
getVars (Not p) = getVars p 
getVars (Or p1 p2) = concatMap (getVars) [p1,p2]
getVars (And p1 p2) = concatMap (getVars) [p1,p2]

getValues :: [String] -> IO [(String,Bool)]
getValues [] = return []
getValues (h:t) = do putStrLn "Qual o valor de " ++ h ++ "?"
                     valor <- getLine 
                     resto <- getValues t 
                     