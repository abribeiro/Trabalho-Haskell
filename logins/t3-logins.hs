import System.IO
import Data.Char

main :: IO ()
main = do
    strcontent <- readFile "nomes.csv"
    let strlist = lines strcontent
        strnew = map (userName) strlist
    writeFile "logins.csv" (unlines (zipWith(++) strlist strnew))

lastName :: String -> String
lastName string = reverse (takeWhile (/= ' ') (reverse string))
	
userName :: String -> String
userName string = ","++[toLower x | x <- [head(string)]] ++ [toLower x | x <- lastName(string)]