-- Trabalho 1 Haskell 
-- Alessandro Bueno Ribeiro

-- 1. Escreva uma função hasEqHeads :: [Int] -> [Int] -> Bool que verifica se as 2 listas possuem o mesmo primeiro elemento. Exemplo de uso:

hasEqHeads :: [Int] -> [Int] -> Bool 
hasEqHeads [] [] = True
hasEqHeads (x:_) (y:_) = if x == y then True else False

-- 2. Observe a função abaixo, que eleva ao cubo cada elemento da lista, produzindo outra lista.

pot3 :: [Int] -> [Int]
pot3 [] = []
pot3 ns = (head ns)^3 : pot3 (tail ns)

-- 3. Escreva uma função recursiva add10, que adicione a constante 10 a cada elemento de uma lista, produzindo outra lista.
add10 :: [Int] -> [Int]
add10 [] = []
add10 (x:xs) = x+10 : add10 (xs)

-- 4. Escreva uma função recursiva addComma, que adicione uma vírgula no final de cada string contida numa lista. 
-- Dica: (1) Strings são listas de caracteres. (2) Para concatenar listas, use o operador ++.

addComma :: [String] -> [String]
addComma [] = []
addComma x = ((head x) ++ ",") : addComma (tail x)

-- 5. Refaça os 2 exercícios anteriores usando a função de alta ordem 'map'.

add10map :: [Int] -> [Int]
add10map [] = []
add10map x = map (+10) x

addCommap :: [String] -> [String]
addCommap [] = []
addcommap x = map (++ ",") x

-- 6. Crie uma função htmlListItems :: [String] -> [String], que receba uma lista de strings e retorne outra lista contendo as strings formatadas 
--como itens de lista em HTML. Dica: use map e defina uma função auxiliar a ser aplicada a cada elemento. Exemplo de uso da função:
-- > htmlListItems ["abra", "ca", "dabra"]
--["<LI>abra</LI>", "<LI>ca</LI>", "<LI>dabra</LI>"]

htmlListItems :: [String] -> [String]
htmlListItems [] = []
htmlListItems s = ("<LI>" ++ (head s) ++ "</LI>") : htmlListItems (tail s)

-- 7. Crie uma função recursiva charFound :: Char -> String -> Bool, que verifique se o caracter (primeiro argumento) está contido na string (segundo argumento). 
charFound :: Char -> String -> Bool
charFound _ "" = False -- percorre lista e não encontra o caracter
charFound x y = if x == (head y) then True else charFound x (tail y)

-- 8. Reescreva a função anterior sem recursão, usando outras funções pré-definidas já vistas em aula.
charFound2 :: Char -> String -> Bool
charFound2 x y 
		| (filter (== x) y) == "" = False
		| otherwise = True
			   

-- 9. Use a função de alta ordem 'zipWith' para produzir uma função que obtenha as diferenças, par a par, 
-- dos elementos de duas listas. Por exemplo: para listas de entrada [1,2,3,4] e [2,2,1,1], o resultado será [-1,0,2,3].

diferencas2list :: [Int] -> [Int] -> [Int]
diferencas2list _ [] = []
diferencas2list x y =(zipWith (-) x y)

-- Funções de alta ordem 

-- 1.Dada uma lista de números, calcular 2*n+1 para cada número n contido na lista. 

lambda1 = \x -> (x*2) + 1

funclambda1 :: [Int]->[Int]
funclambda1 []=[]
funclambda1 n = map lambda1 n

-- 2. Dadas duas listas X e Y de números inteiros, calcular 4*x+2*y+1 para cada par de números x e y pertencentes às listas. Exemplo:

auxFunc :: (Int,Int) -> Int
auxFunc (x,y) = ((4*x) +(2*y) +1)

mapfuncxy :: [(Int,Int)] -> [Int]
mapfuncxy [] = []
mapfuncxy (x:xs) = (auxFunc x) : mapfuncxy xs

-- 3. Dada uma lista de strings, produzir outra lista com strings de 10 caracteres, usando o seguinte esquema:
-- strings de entrada com mais de 10 caracteres são truncadas, strings com até 10 caracteres são completadas com '.' até ficarem com 10 caracteres. 
-- Exemplo: > func ["palavras","paralelas","pedal","paralelepipedo"]
-- ["palavras..","paralelas.","pedal.....","paralelepi"]
-- calcula tamanho de uma string


stringList :: [String] -> [String]
stringList [] = []
stringList (x:xs) 
			|length x >= 10 = (take 10 x) : stringList (xs)
            |length x < 10 = (x ++ (replicate (10 - (length x)) '.')) : compString (xs)

			  
-- 4. Dada uma lista de idades, selecionar as que são maiores que 20 e, para cada uma, calcular o ano de nascimento correspondente (aproximado, considerando o ano atual).

idcalc :: Int->Int
idcalc x = 2015-x

aux2 :: Int -> Bool
aux2 x = (x > 20)

idadeFunc :: [Int]->[Int]
idadeFunc []=[]
idadeFunc x =  map idcalc (filter aux2 x)