import System.Random(randomRIO)

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

embaralha_letras :: (Int, [String]) -> [String]
embaralha_letras (n,[]) = []
embaralha_letras (n,p:ps) 
  | tam < 10 =
    do
      let x = (randomRs (1, 50) (mkStdGen (n)))!!tam :: Int
      let l = (randomRs ('a', 'z') (mkStdGen (n)))!!tam :: Char
      if(x `mod` 2 == 0) then
        embaralha_letras(n*x, (p++[l]):ps)
      else
        embaralha_letras(n*x, ([l]++p):ps)
      
  | otherwise = [p]++embaralha_letras(n-tam,ps)
    where tam = length(p)


imprimeMatriz :: [String] -> IO()
imprimeMatriz [] = putStrLn""
imprimeMatriz (m:ms) = do
  putStrLn m
  imprimeMatriz(ms)

vertical :: ([String], String, Int, Int, Int) -> [String]
vertical (m:ms, "", i, j, cont) = m:ms
vertical (m:ms, p:ps, i, j, cont)
  | cont == i =
    if(tam == j) then
      [m++[p]] ++ vertical(ms, ps, i, j, cont)
    else
      if(m == "" || length(tail(m)) == 0) then
        vertical((m++"1"):ms, p:ps, i, j, cont)
      else
        vertical(tail(m):ms, p:ps, i, j, cont)

  | otherwise = vertical(ms, p:ps, i, j, cont+1)
  where tam = length(m)