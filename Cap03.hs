module Cap03 where

-- 3.1) Crie o tipo Pergunta com os values constructors Sim ou Nao.
data Pergunta = Sim | Nao deriving (Show)

-- Faça as funções seguintes, determinando seus tipos explicitamente.

-- pergNum : recebe via parâmetro uma  Pergunta .
-- Retorna 0 para Nao e 1 para Sim.
pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

-- listPergs : recebe via parâmetro uma lista de Perguntas,
-- e retorna 0 s e 1 s correspondentes aos constructores contidos na lista.
listPergs :: [Pergunta] -> [Int]
listPergs [] = []
listPergs (p : ps) = pergNum p : listPergs ps

-- ou --
listPergs' :: [Pergunta] -> [Int]
listPergs' ps = map pergNum ps

-- and' : recebe duas  Perguntas como parâmetro e
-- retorna a tabela  verdade do  and lógico,
-- usando Sim como verdadeiro e Nao como falso.
and' :: Pergunta -> Pergunta -> Pergunta
and' Sim Sim = Sim
and' Nao _ = Nao
and' _ Nao = Nao

-- or' : idem ao anterior, porém deve ser usado o ou lógico.
or' :: Pergunta -> Pergunta -> Pergunta
or' Nao x = x
or' x Nao = x
or' Sim _ = Sim

-- not' : idem aos anteriores, porém usando o  not lógico.
not' :: Pergunta -> Pergunta
not' Sim = Nao
not' Nao = Sim

-- 3.2) Faça o tipo Temperatura que pode ter valores Celsius, Farenheit ou Kelvin.
data Temperatura = Celsius | Farenheit | Kelvin

-- Implemente as funções:
-- converterCelsius : recebe um valor double e uma temperatura,
-- e faz a conversão para Celsius.
converterCelsius :: Double -> Temperatura -> Double
converterCelsius c Celsius = c
converterCelsius f Farenheit = (f - 32) / 1.8
converterCelsius k Kelvin = k - 273.15

-- converterKelvin : recebe um valor double e uma temperatura,
-- e faz a conversão para Kelvin.
converterKelvin :: Double -> Temperatura -> Double
converterKelvin k Kelvin = k
converterKelvin c Celsius = c + 273.15
converterKelvin f Farenheit = converterKelvin (converterCelsius f Farenheit) Celsius

-- converterFarenheit : recebe um valor  double e uma temperatura,
-- e faz a conversão para Farenheit.
converterFarenheit :: Double -> Temperatura -> Double
converterFarenheit f Farenheit = f
converterFarenheit c Celsius = 1.8 * c + 32
converterFarenheit k Kelvin = converterFarenheit (converterCelsius k Kelvin) Celsius

-- 3.3) Implemente uma função que simule o vencedor de uma partida
-- de pedra, papel e tesoura usando tipos criados.
-- Casos de empate devem ser considerados em seu tipo.
data Escolha = Pedra | Papel | Tesoura deriving (Show)

data Resultado = Vencedor Escolha | Empate deriving (Show)

jogar :: (Escolha, Escolha) -> Resultado
jogar (Pedra, Pedra) = Empate
jogar (Papel, Papel) = Empate
jogar (Tesoura, Tesoura) = Empate
jogar (Pedra, Tesoura) = Vencedor Pedra
jogar (Tesoura, Pedra) = Vencedor Pedra
jogar (Pedra, Papel) = Vencedor Papel
jogar (Papel, Pedra) = Vencedor Papel
jogar (Tesoura, Papel) = Vencedor Tesoura
jogar (Papel, Tesoura) = Vencedor Tesoura

-- 3.4) Faça uma função que retorne uma string,
-- com todas as vogais maiúsculas e minúsculas eliminadas
-- de uma string passada por parâmetro usando list compreenshion
trimVogais :: String -> String
trimVogais s = [c | c <- s, c `notElem` "aeiouAEIOU"]

-- 3.5) Sabe-se que as unidades imperiais de comprimento podem ser
-- Inch , Yard ou  Foot (há outras ignoradas aqui).
-- Sabe-se que 1in=0.0254m , 1yd=0.9144m , 1ft=0.3048.
--
-- Faça a função converterMetros que recebe a unidade imperial
-- e o valor correspondente nesta unidade.
-- Esta função deve retornar o valor em metros.
data Imperial = Inch | Yard | Foot

converterMetros :: Double -> Imperial -> Double
converterMetros x Inch = 0.0254 * x
converterMetros x Yard = 0.9144 * x
converterMetros x Foot = 0.3048 * x

-- Implemente também a função  converterImperial,
-- que recebe um valor em metros e a unidade de conversão.
-- Esta função deve retornar o valor convertido para a unidade desejada.
converterImperial :: Double -> Imperial -> Double
converterImperial x Inch = 39.3701 * x
converterImperial x Yard = 1.093613 * x
converterImperial x Foot = 3.28084 * x

-- 3.6) Faça um novo tipo chamado  Mes, que possui como valores todos os meses do ano.
data Mes = Jan | Fev | Mar | Abr | Mai | Jun | Jul | Ago | Set | Out | Nov | Dez deriving (Show)

mesComTrintaDias :: Mes -> Bool
mesComTrintaDias Abr = True
mesComTrintaDias Jun = True
mesComTrintaDias Set = True
mesComTrintaDias Nov = True
mesComTrintaDias _ = False

-- Implemente:
-- A função checaFim,
-- que retorna o número de dias que cada mês possui (considere fevereiro tendo 28 dias).
checaFim :: Mes -> Int
checaFim Fev = 28
checaFim m = if mesComTrintaDias m then 30 else 31

-- A função prox,
-- que recebe um mês atual e retorna o próximo mês.
prox :: Mes -> Mes
prox Jan = Fev
prox Fev = Mar
prox Mar = Abr
prox Abr = Mai
prox Mai = Jun
prox Jun = Jul
prox Jul = Ago
prox Ago = Set
prox Set = Out
prox Out = Nov
prox Nov = Dez
prox Dez = Jan

-- A função estacao,
-- que retorna a estação do ano de acordo com o mês e com o hemisfério.
data Estacao = Primavera | Verao | Outono | Inverno deriving (Show)

data Hemisferio = N | S deriving (Show)

mesDeVerao :: Mes -> Hemisferio -> Bool
mesDeVerao Jan S = True
mesDeVerao Fev S = True
mesDeVerao Mar S = True
mesDeVerao _ S = False
mesDeVerao Jul N = True
mesDeVerao Ago N = True
mesDeVerao Set N = True
mesDeVerao _ N = False

mesDeOutono :: Mes -> Hemisferio -> Bool
mesDeOutono Abr S = True
mesDeOutono Mai S = True
mesDeOutono Jun S = True
mesDeOutono _ S = False
mesDeOutono Out N = True
mesDeOutono Nov N = True
mesDeOutono Dez N = True
mesDeOutono _ N = False

mesDeInverno :: Mes -> Hemisferio -> Bool
mesDeInverno Jul S = True
mesDeInverno Ago S = True
mesDeInverno Set S = True
mesDeInverno _ S = False
mesDeInverno Jan N = True
mesDeInverno Fev N = True
mesDeInverno Mar N = True
mesDeInverno _ N = False

mesDePrimavera :: Mes -> Hemisferio -> Bool
mesDePrimavera Out S = True
mesDePrimavera Nov S = True
mesDePrimavera Dez S = True
mesDePrimavera _ S = False
mesDePrimavera Abr N = True
mesDePrimavera Mai N = True
mesDePrimavera Jun N = True
mesDePrimavera _ N = False

estacao :: Mes -> Hemisferio -> Estacao
estacao m h
  | mesDeVerao m h = Verao
  | mesDeOutono m h = Outono
  | mesDeInverno m h = Inverno
  | mesDePrimavera m h = Primavera

-- 3.7) Faça uma função que receba uma  String
-- e retorne True se esta for um palíndromo; caso contrário, False.
palindromo :: String -> Bool
palindromo s = s == reverse s

-- 3.8) Faça uma função que elimine todos os números pares,
-- todos os ímpares múltiplos de 7 e negativos de uma lista
-- de inteiros passada via parâmetro.
-- Você deve retornar esta lista em ordem reversa
-- em comparação a do parâmetro.
func3_8 :: [Int] -> [Int]
func3_8 xs = reverse [x | x <- xs, odd x && mod x 7 /= 0 && x > 0]

-- 3.9) Faça uma função que recebe três Strings x , y e z como parâmetro.
-- A função retorna uma tupla com três coordenadas
-- contendo a ordem reversa em cada.
-- A primeira coordenada deve conter string reversa
-- do primeiro parâmetro, e assim por diante.
func3_9 :: String -> String -> String -> (String, String, String)
func3_9 x y z = (reverse x, reverse y, reverse z)

-- 3.10) Faça uma função chamada  revNum ,
-- que receba uma String s e um Int n.
-- Esta deverá retornar as n primeiras letras
-- em ordem reversa e o restante em sua ordem normal.
-- Exemplo: revNum 4 "FATEC" = "ETAFC"
revNum :: String -> Int -> String
revNum s n = reverse (take n s) ++ drop n s
