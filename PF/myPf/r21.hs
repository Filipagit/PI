type TabTemp = [(Cidade,Data,Temp,Temp)]   -- (cidade, data, temp. mínima, temp. máxima) 

type Data = (Int,Int,Int)                  -- (ano, mês, dia)

type Temp = Float

type Cidade = String


--1 dadas uma tabela de temperaturas, constroi a lista com as temperaturas médias de cada dia.
medias :: TabTemp ->  [(Cidade,Data,Temp)]
medias [] = []
medias ((c,d,tmin,tmax):t)= (c,d,(tmax+tmin)/2): medias t 


--2 ada uma tabela de temperaturas e duas datas, cria uma tabela com as temperaturas registadas entre essas duas datas.
--fun :: TabTemp -> Data -> Data -> TabTemp
--fun [] _ _ =[] 
--fun ((c,(a,m,d),tmax,tmin):t) (a1,m1,d1) (a2,m2,d2) | a==a1 && m<m1 || a==a1 && m==m1 && d<d1 || a>a2 || a==a2 && m>m2 || a==a2 && m==m2 && d>d2  = fun t (a1,m1,d1) (a2,m2,d2) 
                  --                                  | a1==a2 && m2<m1 || a1==a2 && m1==m2 && d2<d1 = []
                --                                    |otherwise = (tmax,tmin): fun t (a1,m1,d1) (a2,m2,d2)

--3 
--flip :: Imagem -> Imagem 
--flip [[]] = [[]]
--flip m = m1 : flip m2
  --  where m1=map head m 
    --     m2 = map tail m    

--4 
data Pokedex = Empty | Pokemons Pokemon Pokedex Pokedex

type Pokemon = (Nome, Stamina, Tipo)

data Tipo = Agua | Fogo | Terra | Ar

type Nome = String 

type Stamina = Int 

 
--que dado um inteiro, determina a lista de todos os nomes de pokémons e a sua stamina, sempre que a stamina seja menor que dito inteiro. 
--A lista deve estar ordenada de forma decrescente pela stamina.  
--Por exemplo, pokemons 92 p1 deverá retornar [(“Psyduck”,90),(“Charmander”,55)]
pokemons :: Int -> Pokedex -> [(Nome,Int)]
pokemons _ Empty = []
pokemons num (Pokemons (nome,stamina,tipo) ae ad) | num > stamina = (nome,stamina) : pokemons num ae  
                                                  | num < stamina = pokemons num ad