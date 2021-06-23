--exercicio 3 
data Contacto = Casa Integer
| Trab Integer
| Tlm Integer
| Email String
deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

--3a 
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email agenda = agenda ++ [(nome,Email email)]

--3b
verEmails :: Nome -> Agenda -> Maybe[String]
verEmails _ [] = Nothing
verEmails nome ((n,c):t) = if(nome==n)
	                       then Just(emails c)
	                       else verEmails nome t
where emails :: [Contacto] -> [String]
           emails [] = []
           emails (Email x:t) = x: emails t
           emails (_:t) = emails t
--3c
consTelefs :: [Contacto] -> [Integer]
consTelefs (x:xs) = case x of 
                  Casa n -> n:consTelefs xs
                  Trab n -> n:consTelefs xs
                  Tlm n -> n: consTelefs xs
                  _ -> consTelefs xs 

--3d
casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa nome ((n,c):t)= if(nome==n)
                     then aux c
                     else casa nome t  
             where aux :: [Contacto] -> Maybe Integer
           aux (x:xs) = case x of 
                      Casa num -> Just num
                      _ -> aux xs

--4
type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String
data Data = D Dia Mes Ano
deriving Show
type TabDN = [(Nome,Data)]

--4a indica a data de nascimento de uma pessoa conseguiii
procura :: Nome -> TabDN -> Maybe Data   
procura _ [] = Nothing      
procura name ((no,d):t)= if name ==no
                         then Just d 
                         else procura name t        

--4b calcula a idade da pessoa numa dada data 
idade :: Data -> Nome -> TabDN -> Maybe Int 
idade (D d m a) nome ((no,D d1 m1 a1):t) = if nome == no
                                         then if m>m1 || m==m1 && d>d1
                                              then Just(a-a1)-- já fez anos 
                                              else Just((a-a1)-1) --ainda nao fez anos 
                                         else idade (D d m a) nome t                                   

--4c testa se uma data é anterior a outra conseguiii
anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2) =  if a1<a2 || a1==a2 && m1<m2 || a1==a2 && m1==m2 && d1<d2
	                                  then True 
	                                  else False

--4d ordena uma tabela de datas por ordem crescente de datas de nascimento 	                                   
ordena :: TabDN -> TabDN 
ordena [] = []
ordena (x:xs)= insereN x (ordena xs)
	where insereN :: (Nome,Data) -> TabDN ->TabDN
	      insereN (n,d) [] = [(n,d)]
	      insereN (n,d) ((no,d1):t) = if anterior d d1
	      	                          then (n,d):(no,d1):t
	      	                          else (no,d1): insereN (n,d):t

--4e conseguiii
porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] =[]
porIdade (D d m a) tab= porIdade (D d m a) ordena(tab)
 
 