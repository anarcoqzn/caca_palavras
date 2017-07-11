--import System.Random(randomRIO)

main = do 
        putStrLn " "
        putStrLn "*********************JOGO DE CAÇA PALAVRAS*********************"
        putStrLn " "
        putStrLn "Intruções: "
        putStrLn "   1 - Você irá digitar o número de palavras que devem ser escondidas no caça palavras."
        putStrLn "   2 - Você deverá digitar as palavras que serão escondidas uma por vez."
        putStrLn "   3 - Para procurar as palavras você irá precisar informar as linhas e as colunas das palavras que será formada."
        putStrLn "   4 - O jogo será finalizado quando todas as palavras forem encontradas."
        putStrLn " "
        putStrLn "Digite a quantidade de palavras que serão escondidas:"
        qnt_palavras <- readLn
        valida_n(qnt_palavras)
        
        --input <- getLine
    	--let palavras = (words input)
	    --imprimeMatriz(caca_palavra)
        --x <- randomRIO (0,9::Int)
        --y <- randomRIO (0,9::Int)
        imprimeMatriz(embaralha_letras(["","","","","agua","","","","",""]))
        
--caca_palavra = ["","","","","dfgaguadfg","","","","",""]

valida_n :: Int -> IO()
valida_n n
    | n < 4 = do
              putStrLn "A quantidade de palavras deve ser maior ou igual a 4, tente outra quantidade."   
              x <- getLine
              let qnt_palavras = read x :: Int
              valida_n(qnt_palavras) 
    | n > 25 = do
               putStrLn "A quantidade de palavras deve ser menor ou igual a 25, tente outra quantidade."
               x <- getLine
               let qnt_palavras = read x :: Int
               valida_n(qnt_palavras)
    | otherwise = putStrLn ""

imprimeMatriz :: [String] -> IO()
imprimeMatriz [] = putStrLn""
imprimeMatriz (m:ms) = do
	putStrLn m
	imprimeMatriz(ms)
	
embaralha_letras :: [String] -> [String]
embaralha_letras [] = []
embaralha_letras (palavra:ms) 
	| length(palavra) == 10 = [palavra] ++ embaralha_letras(ms)
	| otherwise = do
					let indice = 13 -- randomico
					if(indice `mod` 2 == 0) then do
						let temp = palavra ++ [['a'..'z']!!indice]
						embaralha_letras(temp:ms)
					else do
						let temp = [['a'..'z']!!indice] ++ palavra
						embaralha_letras(temp:ms)
