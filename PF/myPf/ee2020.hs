--1 conseguiii
subst:: Eq a => (a,a) -> [a] -> [a]
subst _ [] = []
subst (x,y) (h:t) = if x==h 
	                then y: subst (x,y) t 
	                else h:subst (x,y) t 

--2 conseguiiii proud of meeeeeeeeeee
posicoes :: [a] -> [Int] -> [a]
posicoes _ []= []
posicoes l (h:t)= indicapos l h ++ posicoes l t 
	
indicapos :: [a] -> Int-> [a]
indicapos [] _  = [] 
indicapos (h:t) n = if n==1 
	                then [h] 
	                else indicapos t (n-1)


--2 
data Tree a b = Leaf b | Node a (Tree a b) (Tree a b)

--a lista as folhas da arvore (esq to dir )
folhas :: Tree a b -> [b]
folhas Leaf x = [x]
folhas Node x ((Tree e d) Empty) = x:folhas e : folhas d 
folhas Node x  e d =  x:folhas e : folhas d 





