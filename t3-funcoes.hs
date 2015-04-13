import Data.Char

-- Trabalho 3 Haskell 
-- Alessandro Bueno Ribeiro

--1. Escreva uma função recursiva firstName :: String -> String que, dado o nome completo de uma pessoa, 
--obtenha seu primeiro nome. Suponha que cada parte do nome seja separada por um espaço e que não existam 
--espaços no início ou fim do nome.

firstName :: String -> String
firstName (x:xs) | x == ' ' = [] 
				 | otherwise = firstName xs 

--2.Escreva uma função firstName' :: String -> String com o mesmo resultado do exercício anterior, mas sem usar recursão. 
--Dica: estude funções pré-definidas em Haskell (List operations -> Sublists) em 
--http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html.

firstName' :: String -> String
firstName' x = takeWhile (/=' ') x


-- 3. Escreva uma função lastName :: String -> String que, dado o nome completo de uma pessoa, 
--obtenha seu último sobrenome. Suponha que cada parte do nome seja separada por um espaço e
-- que não existam espaços no início ou fim do nome.

lastName' :: String -> String
lastName' = last . words

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

----------------------------------------------------
-- segunda resolução --


userName' :: String -> String
userName' = getLogin . words . map toLower
   where
      getLogin xs = (head . head $ xs) : last xs

-- função que chama o exercicio 3 e devolve formato de login : primeira
--letra do nome mais o ultimo sobrenome

--5. Escreva uma função não-recursiva encodeName :: String -> String que substitua vogais em uma string, 
--conforme o esquema a seguir: a = 4, e = 3, i = 1, o = 0, u = 00.

encodeName :: String -> String
encodeName [] = []
encodeName (x:xs) = vog2num x ++ encodeName xs
   where
      vog2num c | c == 'a'  = "4"
                | c == 'e'  = "3"
                | c == 'i'  = "1"
                | c == 'o'  = "0"
                | c == 'u'  = "00"
                | otherwise = c:[]

--6. Escreva uma função isElem :: Int -> [Int] -> Bool que verifique se um dado elemento pertence a uma lista, 
--conforme os exemplos abaixo:
-- -- > isElem 4 [3,4,5,6,7]
--True

isElem _ [] = False
isElem x (y:ys) | x == y    = True
                | otherwise = isElem x ys

isElem' :: Int -> [Int] -> Bool
isElem' x = any (== x)


-- 7. Escreva uma função recursiva que retorne o número de vogais em uma string.
vogais :: String -> Int
vogais [] = 0
vogais x = (cont (head x)) + (vogais (tail x))
  where 
      cont x | x == 'a' = 1
             | x == 'e' = 1
             | x == 'i' = 1
             | x == 'o' = 1
             | x == 'u' = 1
             | x == 'A' = 1
             | x == 'E' = 1
             | x == 'I' = 1
             | x == 'O' = 1
             | x == 'U' = 1
             | otherwise = 0

-- 8. Escreva uma função não-recursiva que retorne o número de consoantes em uma string.

consoantes :: String -> Int  
consoantes x = length (filter (\ x -> (x /= 'a') && (x /= 'e') && (x /= 'i') && (x /= 'o') && (x /= 'u') && (x /= 'A') && (x /= 'E') && (x /= 'I') && (x /= 'O') && (x /= 'U') && (x /= ' ')) x)

-- 9. Escreva uma função não-recursiva que verifique se uma dada string só contém dígitos (0 a 9).
digNove :: String -> Bool
digNove [] = False 
digNove x = if length (x)/= length (filter (\x-> (x == '0') || (x == '1') || (x == '2') || (x == '3') || (x == '4') || (x == '5') || (x == '6') || (x == '7') || (x == '8') || (x == '9') && (x /=' ')) x) then False else True

-- 10. Escreva uma função não-recursiva que converta uma string em um número inteiro, fazendo operações aritméticas com seus dígitos (p.ex.: "356" = 3*100 + 5*10 + 6*1 = 356). Considere que a string seja um número válido, isto é, só contenha dígitos de 0 a 9.

strToInt :: String -> Int
strToInt x = sum $ zipWith (*) (map (10^) [(length x) -1, (length x) -2 .. 0]) $ map (digitToInt) x