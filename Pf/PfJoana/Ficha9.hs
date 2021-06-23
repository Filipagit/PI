module Ficha9 where

import Data.List
import System.IO
import System.Random
import System.IO.Error

---1
--a
bingo :: IO ()
bingo = bingoAux []

bingoAux :: [Int] -> IO ()
bingoAux l = if length l == 90
             then putStrLn "End!"
             else do n <- randomRIO (1,90)
                     if elem n l 
                     then bingoAux l
                     else do putStrLn (show n)
                             bingoAux (n:l)

--b
mastermind :: IO ()
mastermind = do seq <- geraSeq
                putStrLn "Escreva a sua sequencia:"
                aposta <- getLine
                ap <- return (read aposta :: [Int])
                comparar ap seq 

geraSeq :: IO [Int]
geraSeq = do n1 <- randomRIO (0,9)
             n2 <- randomRIO (0,9)
             n3 <- randomRIO (0,9)
             n4 <- randomRIO (0,9)
             return [n1,n2,n3,n4]

comparar :: [Int] -> [Int] -> IO ()
comparar ap seq = do putStrLn ("Numero de digitos corretos na posicao correta:" ++ show a)
                     putStrLn ("Numero de digitos corretos na posicao errada:" ++ show b)
                  where a = somarCorretos ap seq 
                        b = somarErrados (tirarCorretos ap seq) seq 

somarCorretos :: [Int] -> [Int] -> Int
somarCorretos (x:xs) (y:ys) = if x==y then 1 + somarCorretos xs ys 
                                      else somarCorretos xs ys
somarCorretos _ _ = 0

tirarCorretos :: [Int] -> [Int] -> [Int]
tirarCorretos (x:xs) (y:ys) = if x==y then tirarCorretos xs ys 
                                      else x: tirarCorretos xs ys
tirarCorretos _ _ = []

somarErrados :: [Int] -> [Int] -> Int
somarErrados (x:xs) l = if elem x l then 1 + somarErrados xs l  
                                    else somarErrados xs l
somarErrados _ _ = 0  

---2
data Aposta = Ap [Int] (Int,Int)

--a
valida :: Aposta -> Bool
valida (Ap l (x,y)) = (length l == 5) && (all (\n -> elem n [1..50]) l) && (elem x [1..9]) && (elem y [1..9]) && (nub l == l) && (nub [x,y] == [x,y])

--b
comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap l1 (x,y)) (Ap l2 (z,w)) = (n,e)
                                 where n = length (intersect l1 l2)
                                       e = length (intersect [x,y] [z,w]) 

--c
--i
instance Eq Aposta where
    (==) ap1 ap2 = (comuns ap1 ap2) == (5,2) 

--ii
premio :: Aposta -> Aposta -> Maybe Int
premio ap ch = case (comuns ap ch) of 
               (5,2) -> Just 1
               (5,1) -> Just 2
               (5,0) -> Just 3
               (4,2) -> Just 4
               (4,1) -> Just 5 
               (4,0) -> Just 6
               (3,2) -> Just 7
               (2,2) -> Just 8
               (3,1) -> Just 9
               (3,0) -> Just 10
               (1,2) -> Just 11
               (2,1) -> Just 12
               (2,0) -> Just 13
               _ -> Nothing

--d
--i
leAposta :: IO Aposta
leAposta = do n <- getLine
              ap <- return (read n :: [Int]) 
              if valida (converteAp ap) 
              then return (converteAp ap) 
              else do putStrLn "Aposta Invalida"
                      leAposta 

converteAp :: [Int] -> Aposta
converteAp l = Ap (take 5 l) (head (drop 5 l), last l)

--ii
joga :: Aposta -> IO ()
joga ch = do ap <- leAposta
             case (premio ap ch) of 
              (Just m) -> putStrLn ("Premio:" ++ show m)
              Nothing -> putStrLn "Sem Premio"

--e
geraChave :: IO Aposta
geraChave = do n <- geraNum
               e <- geraEstrelas
               if valida (Ap n e)
               then return (Ap n e)
               else geraChave 

geraNum :: IO [Int]
geraNum = do n1 <- randomRIO (1,50)
             n2 <- randomRIO (1,50)
             n3 <- randomRIO (1,50)
             n4 <- randomRIO (1,50)
             n5 <- randomRIO (1,50)
             return [n1,n2,n3,n4,n5]

geraEstrelas :: IO (Int,Int)
geraEstrelas = do e1 <- randomRIO (1,9)
                  e2 <- randomRIO (1,9)
                  return (e1,e2)

--f
main :: IO ()
main = do ch <- geraChave
          ciclo ch

menu :: IO String
menu = do { putStrLn menutxt
          ; putStr "Opcao: "
          ; c <- getLine
          ; return c
          }
    where menutxt = unlines ["",
                             "Apostar ........... 1",
                             "Gerar nova chave .. 2",
                             "",
                             "Sair .............. 0"]

ciclo :: Aposta -> IO ()
ciclo ch = do menuOpt <- menu
              case menu of "1" -> do joga ch
                                     ciclo ch 
                           "2" -> do putStrLn "Nova chave gerada"
                                     main
                           "0" -> return ()



