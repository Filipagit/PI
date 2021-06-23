--1dada uma lista e um inteiro, calcula o elemento da lista que se encontra nessa posicao (assume-se que o
--primeiro elemento se encontra na posicao)
--conseguiiii
calculael :: [a] -> Int -> a 
calculael (h:t) x = if x==0 
	                then h 
	                else calculael t (x-1)

--2 conseguiii
data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (xi,yi) [] = (xi,yi)
posicao (xi,yi) (Norte:t)= posicao(xi,yi+1) t
posicao (xi,yi) (Sul:t)= posicao (xi,yi-1) t
posicao (xi,yi) (Este:t)=posicao(xi+1,yi) t 	                
posicao (xi,yi) (Oeste:t)=posicao(xi-1,yi) t

--3 conseguii
isany :: (a -> Bool) -> [a] -> Bool
isany f [] = False 
isany f (h:t) = if f h 
	            then True 
	            else isany f t 

--5
movimenta :: IO (Int,Int)
movimenta =movimentaAux (0,0)

movimentaAux :: (Int,Int) -> IO(Int,Int)
movimentaAux (x,y) = do direcao <-getChar
                        case direcao of 'N' -> movimentaAux(x,y+1)
                                        'S' -> movimentaAux (x,y-1)
                                        'E' -> movimentaAux (x+1,y)
                                        'O' -> movimentaAux (x-1,y)
                                        otherwise -> return (x,y)

--6
data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]                                         

--a conseguiii
vazia :: Imagem -> Bool
vazia im = if(conta im ==0)
	       then True 
	       else False 

--conta o nr de quadrados 
conta :: Imagem -> Int
conta (Quadrado _) = 1 
conta (Mover (_,_) im) = conta im 
conta (Juntar l) = sum (map conta l)             

--alternativa 
empty :: Imagem -> Bool
empty (Quadrado _) = False 
empty (Mover (_,_) im) = empty im 
empty (Juntar l) | null l = True
                 | otherwise = or (map vazia l) 

--b conseguii
maior :: Imagem -> Maybe Int
maior (Quadrado n) = Just n 
maior (Mover (_,_) im) = maior im 
maior (Juntar []) = Nothing 
maior (Juntar l) = maximum (map maior l)  

--c
                
instance Eq Imagem where
    img1 == img2 = posQuadrado img1 (0,0) == posQuadrado img2 (0,0)

posQuadrado :: Imagem -> (Int,Int) -> [(Int,(Int,Int))]
posQuadrado (Quadrado n) (x,y) = [(n,(x,y))]
posQuadrado (Mover (a,b) im) (x,y) = posQuadrado im (a+x,b+y)
posQuadrado (Juntar l) (x,y) = concatMap (\a -> posQuadrado a (x,y)) l

   
                