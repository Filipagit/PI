import Data.Char 
import Data.List

func :: Int -> [Int] -> Int 
func x l = foldr (+) 0 (filter even l)