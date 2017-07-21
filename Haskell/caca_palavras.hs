import System.Random hiding (split)

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
    x1 <- randomRIO (0,9999::Int)
    
    -- recebe, valida e encaixa palavras
    palavra1 <- getLine
    verificaInteiro(palavra1)
    verificaTamanho(palavra1)
    let t1 = vertical(["","","","","","","","","",""], palavra1, 0,1,0)
    let palavras1 = [palavra1] 
        
    input <- getLine
    let palavra2 = inverte(input)
    verificaInteiro(palavra2)
    verificaTamanho(palavra2)
    verificaRepetida(palavra2, palavras1)
    let t2 = horizontal(t1, palavra2, 0,2,0)    
    let palavras2 = [palavra1, palavra2] 
        
    palavra3 <- getLine
    verificaInteiro(palavra3)
    verificaTamanho(palavra3)
    verificaRepetida(palavra3, palavras2)
    let t3 = diagonal(t2, palavra3, 2, 2, 0)    
    let palavras3 = [palavra1, palavra2, palavra3] 
        
    palavra4 <- getLine
    verificaInteiro(palavra4)
    verificaTamanho(palavra4)
    verificaRepetida(palavra4, palavras3)
    let t4 = horizontal(t3, palavra4, 8, 2, 0)
    let palavras4 = [palavra1, palavra2, palavra3, palavra4]
    
    let t5 = substitui_uns_por_letra(x1, t4)
    let t6 = embaralha_letras(x1+1,t5)
    
    -- imprime caca palavras
    putStrLn "As palavras foram escondidas no caça palavras abaixo!"
    putStrLn " "
    imprimeMatriz(t6)
    putStrLn " "
    
    -- encontrar palavras    
    putStrLn "Agora que as palavras foram escondidas, você deve procurá-las"
    putStrLn "digitando os valores das linhas e colunas correspondentes à cada letra da palavra." 
    putStrLn " "
    encontraPalavra(palavras4, "", t6, 0)

    
palavraNaMatriz :: (String, [String]) -> IO()
palavraNaMatriz(palavra, lista)
    | elem palavra lista = putStr " "
    | otherwise = error "Essa palavra não foi escondida no caça palavras!"

verificaInteiro :: String -> IO()
verificaInteiro [] = putStr ""
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
    | otherwise = putStr ""

verificaRepetida :: (String, [String]) -> IO()
verificaRepetida(palavra, lista)
    | elem palavra lista = error "ERRO: palavra repetida."
    | otherwise = putStr ""

embaralha_letras :: (Int, [String]) -> [String]
embaralha_letras (n,[]) = []
embaralha_letras (n,p:ps) 
  | tam < 10 =
    do
      let l = [(randomRs ('a', 'z') (mkStdGen (n)))!!tam :: Char]
      embaralha_letras(n+2, (p++l):ps)
  | otherwise = [p]++embaralha_letras(n+3,ps)
    where tam = length(p)

substitui_uns_por_letra :: (Int, [String]) -> [String]
substitui_uns_por_letra(_, []) = []
substitui_uns_por_letra(n, m:ms) = troca_uns_por_letra(n, m):substitui_uns_por_letra(n+1,ms)

troca_uns_por_letra :: (Int,String) -> String
troca_uns_por_letra (_, "") = ""
troca_uns_por_letra (n, m:ms)
 | m == '1' = 
  do
    let l = [(randomRs ('a', 'z') (mkStdGen (n)))!!5 :: Char]
    l++troca_uns_por_letra(n+1,ms)
 | otherwise = [m] ++troca_uns_por_letra(n+2, ms)

imprimeMatriz :: [String] -> IO()
imprimeMatriz [] = putStrLn ""
imprimeMatriz (m:ms) = do
  putStrLn("                 " ++ separaLetras(m))
  imprimeMatriz(ms)
  
separaLetras :: String -> String
separaLetras "" = ""
separaLetras(p:ps) = [p] ++ " " ++ separaLetras(ps)
	
vertical :: ([String], String, Int, Int, Int) -> [String]
vertical (m:ms, "", _, _, _) = m:ms
vertical ([], _, _, _, _) = []
vertical (m:ms, p:ps, i, j, contLinha)
  | contLinha == i =
    if(contColuna == j) then
      [m++[p]] ++ vertical(ms, ps, i+1, j, contLinha+1)
    else if(contColuna < j) then
      vertical((m++"1"):ms, p:ps, i, j, contLinha)
      else do 
        let inicio = take j m
        let fim = take (contColuna - j) $ drop (j+1) m 
        [inicio ++ [p] ++ fim] ++ vertical(ms, ps, i, j, contLinha)

  | otherwise = [m] ++ vertical(ms, p:ps, i, j, contLinha+1)
  where tam = length(head(ms));contColuna = length(m)
  
diagonal :: ([String], String, Int, Int, Int) -> [String]
diagonal (m:ms, "", _, _, _) = m:ms
diagonal ([], _, _, _, _) = []
diagonal (m:ms, p:ps, i, j, contLinha)
  | contLinha == i =
    if(contColuna == j) then
      [m++[p]] ++ diagonal(ms, ps, i+1, j+1, contLinha+1)
    else
      if(contColuna < j) then
        diagonal((m++"1"):ms, p:ps, i, j, contLinha) -- concateno com 1 aqui pra ter controle sobre o contColuna. Depois esses 1 sao trocados por letras
      else do 
        let inicio = take j m
        let fim = take (contColuna - j) $ drop (j+1) m 
        [inicio ++ [p] ++ fim] ++ diagonal(ms, ps, i, j, contLinha)

  | otherwise = [m] ++ diagonal(ms, p:ps, i, j, contLinha+1)
  where tam = length(head(ms));contColuna = length(m)

horizontal :: ([String], String, Int, Int, Int) -> [String]
horizontal (m:ms, "", _, _, _) = m:ms
horizontal ([], _, _, _, _) = []
horizontal (m:ms, p, i, j, contLinha)
  | contLinha == i =
    if(contColuna == j) then
      [m++p] ++ ms
    else
      if(contColuna < j) then
        horizontal((m++"1"):ms, p, i, j, contLinha)
      else do 
        let inicio = take j m
        let temp = inicio ++ p
        let fim = take (contColuna - length(temp)) $ drop (length(temp)) m 
        [temp ++ fim] ++ ms

  | otherwise = [m] ++ horizontal(ms, p, i, j, contLinha+1)
  where tam = length(head(ms));contColuna = length(m)
  
inverte :: String -> String
inverte "" = ""
inverte (p:ps) = inverte(ps) ++ [p]

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
    
-- Encontra a palavra a partir das linhas e colunas 
encontraPalavra :: ([String], String, [String], Int) -> IO()
encontraPalavra(_,_,_,4) = putStrLn "Voce achou todas as palavras!!"
encontraPalavra(lista, palavra, caca_palavra, n) = do
    print("Digite a linha e a coluna; se desejar parar digite 0")
    input <- getLine
    let indices = (map read $ words input :: [Int])
    if (length(indices) <= 1) then
        if contemPalavra(lista,palavra) then do 
			putStrLn ""
			putStrLn("Parabens, voce encontrou a palavra: " ++ palavra)
			imprimeMatriz(caca_palavra)
			encontraPalavra(lista, "", caca_palavra, n+1)
        else do
			putStrLn("Voce nao conseguiu achar nenhuma palavra, tente novamente!")
			imprimeMatriz(caca_palavra)
			encontraPalavra(lista, "", caca_palavra, n)
    else do
		let i = indices!!0 - 1
		let j = indices!!1 - 1
		let temp = [(caca_palavra!!i)!!j]
		encontraPalavra(lista, palavra ++ temp, caca_palavra, n) 

        
contemPalavra :: ([String], String) -> Bool
contemPalavra([],_) = False
contemPalavra(l:ls, palavra)
	| l == palavra = True
	| otherwise = contemPalavra(ls, palavra)
