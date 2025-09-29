-- 4.1) Faça uma função que
-- retorne a média de um [Double] , usando foldl .
media :: [Double] -> Double
media xs = foldl (\acc cur -> acc + (cur / len)) 0 xs
  where
    len = fromIntegral $ length xs

-- 4.2) Faça uma função que receba uma [String] e retorne
-- todos os elementos palíndromos. Ver exercício 3.7.
palindromo :: String -> Bool
palindromo s = s == reverse s

extraiPalindromos :: [String] -> [String]
extraiPalindromos = filter palindromo

-- 4.3) Implemente uma função que filtre os números pares e
-- outra que filtre os ímpares de uma lista recebida via parâmetro.
filtraPares :: [Int] -> [Int]
filtraPares = filter even

filtraImpares :: [Int] -> [Int]
filtraImpares = filter odd

-- 4.4) Filtre os números primos
-- de uma lista recebida por parâmetro.
ehPrimo :: Int -> Bool
ehPrimo x
  | x < 2 = False
  | otherwise = divisorsCount == 0
  where
    limit = floor $ sqrt $ fromIntegral x
    divisorsCount = length $ filter (== 0) (map (mod x) [2 .. limit])

filtraPrimos :: [Int] -> [Int]
filtraPrimos = filter ehPrimo

-- 4.5) Implemente uma função que receba uma lista de inteiros e
-- retorne o dobro de todos, eliminando os múltiplos de 4.
func4_5 :: [Int] -> [Int]
func4_5 xs = map (* 2) (filter (\x -> mod x 4 /= 0) xs)

-- 4.6) Faça uma função func que receba uma função f de tipo
-- (String -> String) , e uma String s que retorna o reverso
-- de s concatenado com aplicação da função f em s
func4_6 :: (String -> String) -> String -> String
func4_6 f s = reverse s ++ f s

-- 4.7) Crie um tipo Dia contendo os dias da semana.
-- Faça uma função que receba uma lista de Dias e filtre as Terças .
data Dia = Seg | Ter | Qua | Qui | Sex | Sab | Dom deriving (Show, Eq)

func4_7 :: [Dia] -> [Dia]
func4_7 = filter (/= Ter)
