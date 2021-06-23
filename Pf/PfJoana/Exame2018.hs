module Exame2018 where

---1
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) i = if i == 0 then x else (!!!) xs (i-1) 

---2
data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao x [] = x 
posicao (a,b) (x:xs) = case x of 
                       Norte -> posicao (a,b+1) xs 
                       Sul -> posicao (a,b-1) xs 
                       Este -> posicao (a+1,b) xs 
                       Oeste -> posicao (a-1,b) xs 

---3
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) = f x || any' f xs 

---4
type Mat a = [[a]]

{--triSup :: Num a => Mat a -> Bool
triSup [] = True
triSup (x:xs) = all (==0) l && triSup rm
            where l = map head xs 
                  rm = map tail xs
--}
---5
movimenta :: IO (Int,Int)
movimenta = movimentaAux (0,0) 

movimentaAux :: (Int,Int) -> IO (Int,Int)
movimentaAux (x,y) = do dir <- getChar 
                        case dir of 'N' -> movimentaAux (x,y+1) 
                                    'S' -> movimentaAux (x,y-1)
                                    'E' -> movimentaAux (x+1,y)
                                    'O' -> movimentaAux (x-1,y)
                                    otherwise -> return (x,y)

---6
data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem] 

ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5), Quadrado 4, Mover (4,3) (Quadrado 2)])

--a
vazia :: Imagem -> Bool
vazia (Quadrado _) = False
vazia (Mover (_,_) im) = vazia im
vazia (Juntar []) = True 
vazia (Juntar l) = or (map vazia l)

--b
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

   
