
-- Teste
main = do 
	print(acharLetra caca_palavra 1 1)
	print(acharLetra caca_palavra 2 1)
	print(acharLetra caca_palavra 3 1)
	print(acharLetra caca_palavra 4 1)
	
-- Criado para exemplificar 
caca_palavra = ["cra","asd","sjy","ali","asc","nbg","ert","mhj","fdf","asd"]


-- Procurar palavras no caca_palavra

-- Split
split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where rest = split cs delim

-- Transforma em Int a linha e a coluna recebidas como string
linha :: [String] -> Int
linha (b:bs) = read b :: Int

coluna :: [String] -> Int
coluna (b:bs) = read (head bs) :: Int


-- Acha a letra a partir da linha e coluna
acharLetra :: [String] -> Int -> Int -> [Char]
acharLetra lista linha coluna = acharLinha lista linha coluna 1 1

acharLinha :: [String] -> Int -> Int -> Int -> Int -> [Char]
acharLinha (r:rs) linha coluna acumL acumC
	|(acumL < linha) = (acharLinha rs linha coluna (acumL+1) acumC)
	|(acumL == linha && acumC < coluna) = acharColuna r coluna acumC
	|(acumL == linha && acumC == coluna) = [head r]

acharColuna :: [Char] -> Int -> Int -> [Char]
acharColuna (b:bs) coluna acumC
	|(acumC < coluna) = (acharColuna bs coluna (acumC+1))
	|otherwise = [b]
