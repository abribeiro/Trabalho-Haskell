import System.IO

main :: IO ()
main = do
    strcontent <- readFile "nomes.csv"
    let strlist = lines strcontent
        user = login (strlist)
        strnew = [x++"@inf.ufsm.br" | x<-user]
    writeFile "logins.csv" (unlines strnew)

login :: [String] -> [String]
login [] = []
login n = [map toLower ((head x) : (reverse (takeWhile (/= ' ') (reverse x)))) | x <- n] 