import Data.Char
-- Trabalho 3 Haskell 
-- Alessandro Bueno Ribeiro

-- As funções de alta ordem any e all são pré-definidas na biblioteca Prelude do Haskell
--(veja seção Special Folds). Estude e teste essas funções e apresente 2 exemplos de uso de cada uma delas.

--desconto de 25 por cento em produtos com valores superiores a 1000 reais em toda a loja.

--1. Exemplo com any
funcany :: [Float] -> [Float]
funcany [] =[]
funcany x = any (> 1000.0) x

--2. Exemplo com any
isElem :: Int -> [Int] -> Bool
isElem x = any (== x)

capital :: String -> String  
capital "" = "String vazia, oops!"  
capital all@(x:xs) = "A primeira letra de " ++ all ++ " eh " ++ [x]  


primo :: Int -> Bool
primo n = n > 1 && all (\x -> mod n x /= 0) [2..n-1]


--dolar

($) :: (a -> b) -> a -> b  
f $ x = f x 
