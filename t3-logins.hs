import System.IO

main :: IO ()
main = do
    strcontent <- readFile "nomes.csv"
    let strlist = lines strcontent
        strnew = "str1":"str2":strlist
    writeFile "logins.csv" (unlines strnew)