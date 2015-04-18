main :: IO ()
main = do
    strcontent <- readFile infile
    let listofstrlist = map (splitOnChar ',') (lines strcontent)
        strtuplelist = map (\lis -> (head lis, last lis)) listofstrlist
    writeFile outfile (mkHtmlURLItemsDoc "Usuários Cadastrados no NCC" strtuplelist)
    where 
    infile = "logins.csv"
    outfile = "output.html"


-- Esta funÃ§Ã£o deve ser alterada para chamar outras funções que vão construir o documento HTML
mkHtmlURLItemsDoc :: String -> [(String,String)] -> String
mkHtmlURLItemsDoc title lis = head'("Usuarios Cadastrados no NCC") ++ lista'(lis) ++ "</html>"

head' :: String -> String
head' string = "<html>\n<head>\n<title>"++string++"</title>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n</head>\n\n<body>\n\n<ul>\n"

lista' :: [(String,String)] -> String
lista' lista = 
  let 
    string = unlines(map (itemList) lista)++"</ul>\n"
  in string
  
itemList :: (String, String) -> String
itemList string = "<li><a href=\"http://www.inf.ufsm.br/~"++(snd(string))++"\">"++(fst(string))++"</a></li>"


-- Decompoe string usando um caracter delimitador
splitOnChar :: Char -> String -> [String]
splitOnChar x y = auxSplitOnChar x y [[]]

auxSplitOnChar :: Char -> String -> [String] -> [String]
auxSplitOnChar x [] z = reverse (map reverse z)
auxSplitOnChar x (y:ys) (z:zs) = 
  if y == x then 
            auxSplitOnChar x ys ([]:(z:zs)) 
        else 
            auxSplitOnChar x ys ((y:z):zs)