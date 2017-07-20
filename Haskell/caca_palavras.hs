import System.Random
import System.IO.Unsafe

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
        
        x1 <- randomRIO (0,9999::Int)

        let t1 = vertical(["","","","","","","","","",""], "agua", 0,1,0)
  
        let t2 = vertical(t1, "fogao", 0,2,0)
  
        let t3 = vertical(t2, "farmacia", 2, 6, 0)
  
        let t4 = vertical(t3, "legal", 3, 4, 0)

        let t5 = horizontal(t4, "pasta", 8, 1, 0)

        let t6 = substitui_uns_por_letra(x1, t5)

        let t7 = embaralha_letras(x1+1,t6)
        imprimeMatriz(t7)

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
  putStrLn m
  imprimeMatriz(ms)

vertical :: ([String], String, Int, Int, Int) -> [String]
vertical (m:ms, "", _, _, _) = m:ms
vertical ([], _, _, _, _) = []
vertical (m:ms, p:ps, i, j, contLinha)
  | contLinha == i =
    if(contColuna == j) then
      [m++[p]] ++ vertical(ms, ps, i+1, j, contLinha+1)
    else
      if(contColuna < j) then
        vertical((m++"1"):ms, p:ps, i, j, contLinha) -- concateno com 1 aqui pra ter controle sobre o contColuna. Depois esses 1 sao trocados por letras
      else do 
        let inicio = take j m
        let fim = take (contColuna - j) $ drop (j+1) m 
        [inicio ++ [p] ++ fim] ++ vertical(ms, ps, i, j, contLinha)

  | otherwise = [m] ++ vertical(ms, p:ps, i, j, contLinha+1)
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