module Exame2015 where 

---1
type TurmaL = [(Numero,Aluno)]
type Aluno = (Nome,Nota)
type Numero = Int
type Nome = String
type Nota = Float

turma :: TurmaL
turma = [(1,("Ana",9.5)),(2,("Bernardo", 14)),(3,("Catarina", 16)),(4,("Daniela",10)),(5,("Eduardo",8))]

--a
taxaAp :: TurmaL -> Float
taxaAp [] = 0
taxaAp turma = aprovados turma / todos turma

aprovados :: TurmaL -> Float
aprovados [] = 0 
aprovados ((n,(nome,nota)):xs) = if nota>=9.5 then 1 + aprovados xs 
                                              else aprovados xs

todos :: TurmaL -> Float
todos [] = 0 
todos (x:xs) = 1 + todos xs 

--b
top :: Int -> TurmaL -> [String]
top i [] = []
top i l = topAux i (reverse (ordenaAlunos l)) 

topAux :: Int -> TurmaL -> [String]
topAux i [] = []
topAux i ((n,(nome,nota)):xs) = if i>0 then nome:topAux (i-1) xs 
                                       else []

ordenaAlunos :: TurmaL -> TurmaL
ordenaAlunos [] = [] 
ordenaAlunos (x:xs) = insereAluno x (ordenaAlunos xs)

insereAluno :: (Numero,Aluno) -> TurmaL -> TurmaL
insereAluno aluno [] = [aluno]
insereAluno (n,(nome,nota)) ((num,(name,grade)):xs) = if nota<grade then (n,(nome,nota)):(num,(name,grade)):xs
                                                                    else (num,(name,grade)):insereAluno (n,(nome,nota)) xs
--c
{--lNomeMax :: TurmaL -> Int
lNomeMax [] = 0
lNomeMax l = maximum (map ((a, (b,c)) -> length b)) l 
--}

--d
listaT :: TurmaL -> IO ()
listaT [] = return ()
listaT ((nr, (nome, nota)):r) = do putStrLn ((show nr) ++ " " ++ (show nome) ++ " " ++ (avalia nota))
                                   listaT r  

avalia :: Nota -> [Char]
avalia n = if n >= 9.5 
           then show (round n)
           else show "R"

---2
data TurmaA = Al (Numero,Aluno)
            | Fork (Numero,Numero) TurmaA TurmaA

tA :: TurmaA
tA = Fork (1,12) (Fork (1,3) (Al (1, ("Joao" ,12.3))) (Al (3, ("Maria" , 5.4)))) (Fork (6,12) (Al (6, ("Joana" , 9.5))) (Al (12,("Anastacia",18.8))))

--a
toList :: TurmaA -> TurmaL
toList (Al (nr, aluno)) = [(nr,aluno)]
toList (Fork r e d) = toList e ++ toList d 

fromList :: TurmaL -> TurmaA
fromList [x] = Al x 
fromList l = Fork (min,max) (fromList esq) (fromList dir)
        where min = minimum (map fst l)
              max = maximum (map fst l)
              n = div (length l) 2 
              esq = take n l 
              dir = drop n l 

--b ERRADO
lookupA :: TurmaA -> Numero -> Maybe Aluno
lookupA (Al (nr,aluno)) n = if n==nr then Just aluno else Nothing
lookupA (Fork (n1,n2) e d) n = if n>=n1 && n<=n2 then if n<(div n2 2) 
                                                      then lookupA e n
                                                      else lookupA d n
                                                 else Nothing

                                                 


