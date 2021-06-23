--calcula o ultimo elemento da lista conseguiiii
ultimo :: [a] -> a 
ultimo [x] = x
ultimo (h:t) = ultimo t

--2 calcula o comprimento de uma lista conseguiii
comprimento :: [a] -> Int 
comprimento [] = 0
comprimento (h:t) = 1+ comprimento t 

--3 testa se uma lista é vazia conseguiii
vazia :: [a] -> Bool
vazia [] = True
vazia (h:t)= False

--4 init -> deita fora o ultimo elemento conseguiii
retiraul :: [a] -> [a]
retiraul [x] = []
retiraul (h:t) = h:(retiraul t)

--5 concatena duas listas conseguii
junta :: [a] -> [a] -> [a]
junta [] l= l
junta (h:t) l= h:(junta t l)

--6 retira os ultimos elementos deixando os n primeiros conseguiii
nult :: Int -> [a] -> [a]
nult 0 l = []
nult n (h:t)= h:(nult (n-1) t)

--7 verifica se um elemento pertence a lista 
pertence :: Eq a => a -> [a] -> Bool
pertence _ [] = False 
pertence x (h:t) = if(x==h)
	               then True 
	               else pertence x t 