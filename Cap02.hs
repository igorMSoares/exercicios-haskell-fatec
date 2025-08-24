module Cap02 where

-- 2.1) Gere as listas:
-- a) [1,11,121,1331,14641,161051,1771561]
funcA :: [Int]
funcA = [11 ^ n | n <- [0 .. 6]]

-- b) [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,33,34,35,37,38,39]
funcB :: [Int]
funcB = [x | x <- [1 .. 40], mod x 4 /= 0]

-- c) ["AaBB", "AbBB", "AcBB", "AdBB", "AeBB", "AfBB", "AgBB"]
funcC :: [String]
funcC = [take 1 "ABB" ++ s ++ drop 1 "ABB" | s <- map (: []) ['a' .. 'g']]

-- d) [5,8,11,17,20,26,29,32,38,41]
funcD :: [Int]
funcD = [x | x <- [5 .. 41], mod x 3 == 2 && notElem x [14, 23, 35]]

-- e) [1.0,0.5,0.25,0.125,0.0625,0.03125]
funcE :: [Float]
funcE = [1 / 2 ** n | n <- [0 .. 5]]

-- f) [1,10,19,28,37,46,55,64]
funcF :: [Int]
funcF = [1 + 9 * n | n <- [0 .. 7]]

-- g) [2,4,8,10,12,16,18,22,24,28,30]
funcG :: [Int]
funcG = [x | x <- [2, 4 .. 30], x `notElem` [2 * y | y <- 3 : [3 * x + 1 | x <- [2 .. 4]]]]

-- h) ['@','A','C','D','E','G','J','L']
funcH :: [Char]
funcH = [a | a <- ['@' .. 'L'], a `notElem` ['B', 'F', 'H', 'I', 'K']]

-- 2.2) Crie uma função que verifique se o tamanho de uma String é par ou não. Use Bool como retorno.
ehStrPar :: String -> Bool
ehStrPar s = even $ length s

-- 2.3) Escreva uma função que receba um vetor de Strings e retorne uma lista com todos os elementos em ordem reversa
espelhaStrList :: [String] -> [String]
espelhaStrList strs = reverse [reverse s | s <- strs]

-- 2.4) Escreva uma função que receba um vetor de Strings e retorne uma lista com o tamanho de cada String. As palavras de tamanho par devem ser excluídas da resposta.
oddStrLen :: [String] -> [Int]
oddStrLen strs = [length s | s <- strs, odd $ length s]

-- 2.5) Escreva a função head como composição de duas outras.
customHead :: [a] -> a
customHead ls = last $ reverse ls

-- 2.6) Faça uma função que receba uma String e retorne True se esta for um palíndromo; caso contrário, False.
ehPalindromo :: String -> Bool
ehPalindromo s = s == reverse s

-- 2.7) Faça uma função que receba um inteiro e retorne uma tupla, contendo: o dobro deste número na primeira coordenada, o triplo na segunda, o quádruplo na terceira e o quíntuplo na quarta.
multiplicaInt :: Int -> (Int, Int, Int, Int)
multiplicaInt x = (x * 2, x * 3, x * 4, x * 5)
