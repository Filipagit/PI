--1
--a  testa se uma lista esta ordenada por ordem crescente conseguiii
myisSorted :: (Ord a) => [a] -> Bool 
myisSorted [] = True 
myisSorted [x] = True
myisSorted (h:xs:t) = if h<xs 
	                  then myisSorted (xs:t) 
	                  else False

--b que calcula a lista dos prefixos de uma lista conseguiiii
isinits :: [a] -> [[a]]	  
isinits [] = [[]]                
isinits l = isinits(init l) ++ [l]

---2 da o maior elemento de uma lista deelementos do tipo Maybe a. Considere Nothing o menor dos elementos.
--conseguiiii
maximumMB :: (Ord a) => [Maybe a] -> Maybe a 
maximumMB [] = Nothing
maximumMB [Just x]= Just x
maximumMB (Nothing:t)= maximumMB t 
maximumMB (Just x:Nothing:t)= maximumMB (Just x:t)
maximumMB (Just x:Just y:t)= if x> y 
	                         then maximumMB(Just x:t)
	                         else maximumMB(Just y:t)


-----3 
data LTree a = Tip a | Fork (LTree a) (LTree a)

--a da a lista das folhas de uma ´arvore (da esquerda para a direita) conseguiii
listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d 

--b Defina uma instˆancia da classe Show para este tipo que apresente uma folha por cada linha, precedida de tantos pontos quanta a sua profundidade na ´arvore. Veja o
-- > Fork (Fork (Tip 7) (Tip 1)) (Tip 2)
-- ..7
-- ..1
-- .2

instance Show a => Show (LTree a) where
    show (Tip x) = show x ++ "\n"
    show (Fork e d) = mostra 1 e ++ mostra 1 d 

mostra :: Show a => Int -> LTree a -> String
mostra n (Tip x) = replicate n '.' ++ show x ++ "\n"
mostra n (Fork e d) = mostra (n+1) e ++ mostra (n+1) d

--4 
--maxSumInit :: (Num a,Ord a) => [a] -> a
--maxSumInit l = foldl (\acc x -> max (sum x) acc) (sum l) (inits l)

--5
type RelP a = [(a,a)]
type RelL a = [(a,[a])]
type RelF a = ([a], a->[a])

--a conseguiiiiiiiiii
convPL :: (Eq a) => RelP a -> RelL a
convPL [] = []
convPL [(x,y)]= [(x,[y])]
convPL l =juntatd(junta l)

junta ::(Eq a) => RelP a -> RelL a
junta [] = []
junta [(x,y)]=[(x,[y])]
junta ((x,y):(x1,y1):t) =  if x == x1
	                       then (x,[y,y1]) : junta t 
	                       else (x,[y]) : junta ((x1,y1):t)

juntatd :: (Eq a) => RelL a -> RelL a
juntatd [] = []
juntatd [(x,[y])]=[(x,[y])]
juntatd ((x,l1):(y,l2):t)= if x==y 
                           then (x,l1++l2) : juntatd t 
                           else (x,l1) : juntatd ((y,l2):t)                              

