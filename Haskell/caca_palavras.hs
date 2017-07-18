import System.Random (randomRIO)

main = do 
		-- Informacoes
        putStrLn " "
        putStrLn "*********************JOGO DE CAÇA PALAVRAS*********************"
        putStrLn " "
        putStrLn "Intruções: "
        putStrLn "   1 - Você irá digitar as 4 palavras que serão escondidas."
        putStrLn "   2 - Você deverá digitar as palavras que serão escondidas uma por vez."
        putStrLn "        - As palavras não devem possuir inteiros."
        putStrLn "        - As palavras devem possuir entre 2 e 10 letras."
        putStrLn "        - Não deve possuir palavras repetidas."
        putStrLn "   3 - Para procurar as palavras você irá precisar informar as linhas e as colunas das palavras que será formada."
        putStrLn "   4 - O jogo será finalizado quando todas as palavras forem encontradas."
        putStrLn " "
        putStrLn "Digite as palavras seguidas de um enter: "
	
		
		-- recebe palavras
		
        palavra1 <- getLine
        verificaInteiro(palavra1)
        verificaTamanho(palavra1)
        -- embaralha(palavra1)
        
        palavra2 <- getLine
        verificaInteiro(palavra1)
        verificaTamanho(palavra2)
        -- embaralha(palavra2)
        
        palavra3 <- getLine
        verificaInteiro(palavra1)
        verificaTamanho(palavra3)
        -- embaralha(palavra3)
        
        palavra4 <- getLine
        verificaInteiro(palavra1)
        verificaTamanho(palavra4)
        -- embaralha(palavra4)
        let palavras = [palavra1, palavra2, palavra3, palavra4]
        print palavras
        
        
caca_palavra = ["","","","","","","","","",""]

verificaInteiro :: [Char] -> IO()
verificaInteiro [] = putStrLn ""
verificaInteiro (c:cs) 
	| (c == '0') = error "ERRO: palavra possui inteiro."
    | (c == '1') = error "ERRO: palavra possui inteiro."
    | (c == '2') = error "ERRO: palavra possui inteiro."
    | (c == '3') = error "ERRO: palavra possui inteiro."
    | (c == '4') = error "ERRO: palavra possui inteiro."
    | (c == '5') = error "ERRO: palavra possui inteiro."
    | (c == '6') = error "ERRO: palavra possui inteiro."
    | (c == '7') = error "ERRO: palavra possui inteiro."
    | (c == '8') = error "ERRO: palavra possui inteiro."
    | (c == '9') = error "ERRO: palavra possui inteiro."
    | otherwise = verificaInteiro(cs)

verificaTamanho :: [Char] -> IO()
verificaTamanho palavra
	| length palavra < 2 = error "ERRO: palavra possui menos de 2 letras."
	| length palavra > 10 = error "ERRO: palavra possui mais de 10 letras."
	| otherwise = putStrLn ""
