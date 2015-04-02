import Data.Char
-- Trabalho 3 Haskell 
-- Alessandro Bueno Ribeiro

--1. Escreva uma função recursiva firstName :: String -> String que, dado o nome completo de uma pessoa, 
--obtenha seu primeiro nome. Suponha que cada parte do nome seja separada por um espaço e que não existam 
--espaços no início ou fim do nome.

firstName :: String -> String
firstName (x:xs) = if x == ' ' then [] else xs

--2.Escreva uma função firstName' :: String -> String com o mesmo resultado do exercício anterior, mas sem usar recursão. 
--Dica: estude funções pré-definidas em Haskell (List operations -> Sublists) em 
--http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html.

firstName' :: String -> String
firstName' x = takeWhile (/=' ') x


-- 3. Escreva uma função lastName :: String -> String que, dado o nome completo de uma pessoa, 
--obtenha seu último sobrenome. Suponha que cada parte do nome seja separada por um espaço e
-- que não existam espaços no início ou fim do nome.

lastName :: String -> String
lastName x = reverse (takeWhile (/=' ') (reverse x)) -- primeiro faço o reverse da string inteira depois pego a primeira palavra dessa string
--que será o último nome invertido, e por último no primeiro reverse do código, inverto essa string, que é o ultimo nome

-- 4. Escreva uma função não-recursiva userName :: String -> String que, dado o nome completo de uma pessoa,
--crie um nome de usuário (login) da pessoa, formado por: primeira letra do nome seguida do sobrenome,
--tudo em minúsculas. Dica: estude as funções pré-definidas no módulo Data.Char, para manipulação 
--de maiúsculas e minúsculas.
minusc :: String -> String 
minusc [] = []
minusc (x:xs) = toLower x : minusc xs -- função auxiliar devolve string minuscula

userName :: String -> String
userName [] = []
userName (x:xs) = toLower x : minusc (lastName xs) 

-- função que chama o exercicio 3 e devolve formato de login : primeira
--letra do nome mais o ultimo sobrenome

--5. Escreva uma função não-recursiva encodeName :: String -> String que substitua vogais em uma string, 
--conforme o esquema a seguir: a = 4, e = 3, i = 1, o = 0, u = 00.

{-vogal :: Char -> Char
vogal x | x=='a' = '4'  
        | x=='e' = '3'
        | x=='i' = '1'
        | x=='o' = '0'
        | x=='u' = '5'
        | otherwise = x 

encodeName :: String -> String
encodeName (x:xs) = vogal x : encodeName xs-}

--6. Escreva uma função isElem :: Int -> [Int] -> Bool que verifique se um dado elemento pertence a uma lista, 
--conforme os exemplos abaixo:
-- -- > isElem 4 [3,4,5,6,7]
--True

isElem :: Int -> [Int] -> Bool
isElem x lista 
		| filter (==x) lista == [] = False
		| otherwise = True

--7. Escreva uma função recursiva que retorne o número de vogais em uma string.
--8. Escreva uma função não-recursiva que retorne o número de consoantes em uma string.
--9. Escreva uma função não-recursiva isInt :: String -> Bool que verifique se uma dada string só contém dígitos (0 a 9).
--Exemplos:
-- -- > isInt "901"
--True
--- -- > isInt "2014a"
--False