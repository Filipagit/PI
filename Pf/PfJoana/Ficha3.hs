---3
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

--a
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email agenda = agenda ++ [(nome, [Email email])]

--b 
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing
verEmails nome ((a,b):t) | nome == a = Just (emails b)
                         | otherwise = verEmails nome t

     where emails :: [Contacto] -> [String]
           emails [] = []
           emails (Email x:t) = x: emails t
           emails (_:t) = emails t 

--c
consTelefs :: [Contacto] -> [Integer]
consTelefs (x:xs) = case x of 
                  Casa n -> n:consTelefs xs
                  Trab n -> n:consTelefs xs
                  Tlm n -> n: consTelefs xs
                  _ -> consTelefs xs 

--d
casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa no ((n,c):t) = if no== n then aux c 
                              else casa no t

     where aux :: [Contacto] -> Maybe Integer
           aux (x:xs) = case x of 
                      Casa num -> Just num
                      _ -> aux xs 

---4
type Dia = Int
type Mes = Int
type Ano = Int
type Name = String

data Data = D Dia Mes Ano
          deriving Show

type TabDN = [(Name,Data)]

--a
procura :: Name -> TabDN -> Maybe Data
procura _ [] = Nothing
procura nome ((n,d):t) = if nome==n then Just d 
                                    else procura nome t 

--b
idade :: Data -> Name -> TabDN -> Maybe Int                                    
idade (D dx mx ax) nome ((n,D d m a):t) = if nome == n 
                                          then if mx>m || mx==m && dx>d
                                               then Just (ax-a)
                                               else Just ((ax-a)-1)
                                          else idade (D dx mx ax) nome t

--c
anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2) = (a1<a2) || (a1==a2 && d1<d2) || (a1==a2 && d1==d2 && a1<a2) 

--d
ordena :: TabDN -> TabDN
ordena [] = []
ordena (x:xs) = insereN x (ordena xs)
    where insereN :: (Nome,Data) -> TabDN -> TabDN
          insereN x [] = [x]
          insereN (n,d) ((a,b):xs) = if anterior d b then (n,d):(a,b):xs
                                                     else (a,b): insereN (n,d) xs 

--e
porIdade:: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade (D d m a) tabela = porIdade (D d m a) (ordena tabela) 

 