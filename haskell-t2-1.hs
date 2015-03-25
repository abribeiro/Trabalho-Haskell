-- Trabalho 1 Haskell 
-- Alessandro Bueno Ribeiro

-- Parte 1: List comprehension
-- 1.Escreva uma função geraPotencias :: Int -> [Int], que gere uma lista com as potências de 2, com expoente de n até 0, onde n é o argumento para a função. Use a sintaxe de list comprehension. Exemplo de uso da função:
-- > geraPotencias 8
-- [256,128,64,32,16,8,4,2,1]

geraPotencias :: Int -> [Int]
geraPotencias x = [2^n | n <- reverse[1..x]]

-- 2.Escreva uma função addSuffix :: String -> [String] -> [String] usando list comprehension, para adicionar um sufixo às strings contidas numa lista. Exemplo:
-- > addSuffix "@inf.ufsm.br" ["fulano","beltrano"]
--addSuffix :: String -> [String] -> [String]
-- ["fulano@inf.ufsm.br","beltrano@inf.ufsm.br"]

addSuffix :: String -> [String] -> [String]
addSuffix s ls = [x ++ s | x <- ls] -- adiciona x( cada string da lista de string) com s(string) tal que x pertença a ls(lista)


-- 3. Escreva uma função anosDeNascimento :: [Int] -> [Int] que receba uma lista de idades, selecione as
-- que são maiores que 20 e, para cada uma das selecionadas, calcule o ano de nascimento correspondente 
--(aproximado). Use a sintaxe de list comprehension.

anoDeNascimento :: [Int] -> [Int]
anoDeNascimento i = [(2015-x) | x <- i, x>20]