import Data.Char
-- Trabalho 3 Haskell 
-- Alessandro Bueno Ribeiro

-- As funções de alta ordem any e all são pré-definidas na biblioteca Prelude do Haskell
--(veja seção Special Folds). Estude e teste essas funções e apresente 2 exemplos de uso de cada uma delas.

--desconto de 25 por cento em produtos com valores superiores a 1000 reais em toda a loja.
--1.
-- Exemplo com any -verifica se pelo menos um dos elementos da lista é menor que x.
menor :: Int -> [Int] -> Bool
menor x y = any (< x) y

-- Exemplo com any - verefica se o elemento está na lista
isElem :: Int -> [Int] -> Bool
isElem x = any (== x)

--Exemplo com all - mostra primeira letra da string
capital :: String -> String  
capital "" = "String vazia, oops!"  
capital all@(x:xs) = "A primeira letra de " ++ all ++ " eh " ++ [x]  

-- Exemplo com all - verefica se o numero é primo
primo :: Int -> Bool
primo n = n > 1 && all (\x -> mod n x /= 0) [2..n-1]

-- 2.
-- Em Haskell, o símbolo '$' pode ser usado para escrever códigos ainda mais curtos, porque "diminui/elimina" o uso de parênteses.
-- Multiplica por 0.10 ao elemento informado e imprime como lista.
desconto :: Float -> [Float]
desconto x = map ($ x) [(0.9*)]

-- coloca uma virgula no início e no fim de cada string.
virgula :: [String] -> [String]
virgula [] = []
virgula x = map (","++) $ map (++",") x

--3. 
-- soma elementos de uma lista de inteiros
somaTail :: [[Int]] -> [Int]
somaTail lis = map (sum . tail)lis

-- adiciona oi no início da string e tchau no final.
add :: [String] -> [String]
add [] = []
add x = map (a . b) x
	where
		a = (++"@inf.ufsm.br")
		b = ("Meu email eh "++)