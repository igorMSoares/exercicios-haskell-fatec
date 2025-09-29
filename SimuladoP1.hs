module SimuladoP1 where

-- 1. Considere o tipo Produto contendo os campos nome, valor e categoria.
-- Os campos nome e valor são String e Double, respectivamente,
-- ao passo que categoria ´e um tipo Categoria que possui os valores
-- Livro, Brinquedo e Escritorio.
--
-- Crie os dois tipos com instˆancias necess´arias para a solu¸c˜ao deste exerc´ıcio e implemente as funçoes:
-- • extrair :: [Produto] -> [Double] que retorna uma lista com todos os pre ̧cos de produto;
-- • minValor :: [Produto] -> Double que retorna o pre ̧co do produto de valor m ́ınimo;
-- • livrEscr :: [Produto] -> [Produto] que retorna os produtos que n ̃ao sejam brinquedos;
-- • avgLivrEscr :: [Produto] -> Double que retorna a m ́edia dos produtos que n ̃ao s ̃ao brinquedos;
-- • maxMinLivr :: [Produto] -> (Double,Double) que retorna o maior e o menor pre ̧co dos livros;
-- • countBrinq :: [Produto] -> Int que retorna a quantidade de itens de brinquedo.

data Categoria = Livro | Brinquedo | Escritorio deriving (Show, Eq)

data Produto = Produto {nome :: String, valor :: Double, categoria :: Categoria} deriving (Show)

-- extrair :: [Produto] -> [Double] que retorna uma lista com todos os pre¸cos de produto;
extrair :: [Produto] -> [Double]
extrair = map valor

-- minValor :: [Produto] -> Double que retorna o pre¸co do produto de valor m´ınimo;
minValor :: [Produto] -> Double
minValor = minimum . extrair

-- livrEscr :: [Produto] -> [Produto] que retorna os produtos que n˜ao sejam brinquedos;
livrEscr :: [Produto] -> [Produto]
livrEscr = filter (\p -> categoria p /= Brinquedo)

-- avgLivrEscr :: [Produto] -> Double que retorna a m´edia dos produtos que n˜ao s˜ao brinquedos;
avgLivrEscr :: [Produto] -> Double
avgLivrEscr ps = sum ([valor p | p <- filteredPs]) / fromIntegral (length filteredPs)
  where
    filteredPs = livrEscr ps

-- maxMinLivr :: [Produto] -> (Double,Double) que retorna o maior e o menor pre¸co dos livros;
maxMinLivr :: [Produto] -> (Double, Double)
maxMinLivr ps = (maximum livrVals, minimum livrVals)
  where
    livrVals = [valor p | p <- ps, categoria p == Livro]

-- countBrinq :: [Produto] -> Int que retorna a quantidade de itens de brinquedo.
countBrinq :: [Produto] -> Int
countBrinq ps = length [valor p | p <- ps, categoria p == Brinquedo]

-- 2. Sobre monóides:
-- • Considere o tipo data Sozinho = Sozinho, crie uma instˆancia de Monoid para este tipo.
data Sozinho = Sozinho deriving (Show)

instance Semigroup Sozinho where
  _ <> _ = Sozinho

instance Monoid Sozinho where
  mempty = Sozinho

--
-- • Considere o operador bin ́ario:
-- ign :: String -> String -> String
-- ign l m = m
-- O tipo String com esta opera ̧c ̃ao ign e o elemento neutro [] formam um mon ́oide? Justifique sua resposta.
--
-- R.:
--    Não, pois apesar do operador binário ign ser fechado em String e também associativo, caracterizando um Semigrupo, conforme demonstrado por:
--    ign (ign a b) c = ign b c = c
--    ign a (ign b c) = ign a c = c
--    O elemento [] não pode ser usado como elemento neutro pois:
--    ign(a []) = [], ou seja, a <> [] = [], portanto não obedece à definição de elemento neutro que define que (mempty <> a = a) e (a <> mempty = a).
--    Para ser monóide, é necessário definir um elemento diferente de [] que obedeça a definição de elemento neutro para o semigrupo (a :: String, ign).

-- 3. Considere o tipo data Dupla a = Dupla a [Int]
-- • Qual o kind de Dupla Bool?
--
-- R.: Dupla Bool :: *
--
-- • Crie uma instˆancia de Functor para Dupla.
data Dupla a = Dupla a [Int]

instance Functor Dupla where
  fmap g (Dupla x ys) = Dupla (g x) ys

-- • Qual o tipo da express˜ao Dupla ’5’ [0,1]?
--
-- R.: Dupla ’5’ [0,1] :: Dupla Char
--
-- • Qual o tipo da express˜ao Dupla?
--
-- R.: Dupla :: a -> [Int] -> Dupla a
--
-- • Crie uma instˆancia de Show que mostre na tela uma dupla em formato de tuplas do Haskell.
--   Por exemplo: Dupla ’k’ [1,2,3] deverá ser mostrado (k,[1,2,3]).
instance (Show a) => Show (Dupla a) where
  show (Dupla x ys) = "(" ++ show x ++ "," ++ show ys ++ ")"

-- • Faça uma função mostra :: Dupla a -> Either [Int] a
-- que mostra a lista de inteiros caso o seu tamanho seja maior que zero
-- ou o campo de tipo a caso contr´ario.
mostra :: Dupla a -> Either [Int] a
mostra (Dupla x []) = Right x
mostra (Dupla x ys) = Left ys

-- 4. Dê o tipo das express˜oes abaixo da maneira mais gen´erica poss´ıvel:
-- (a) \x -> x
-- R.: (\x -> x) :: a -> a
--
-- (b) id . tail $ "HELLO"
-- R.:
-- (tail :: [a] -> [a]) e ("HELLO" :: String), então, aplicando tail à "HELLO", ([a] = String) e (a = Char), portanto (tail $ "HELLO") :: [Char]
-- (id :: b -> b) então, aplicando o resultado de (tail $ "HELLO") à (id), temos que (b = [Char] = String)
-- conclui-se, portanto, que (id . tail $ "HELLO") :: [Char]
--
-- (c) 4*9
-- R.: (4*9) :: Num a => a
--
-- (d) ("FATEC", False, 'K')
-- R.: ("FATEC", False, 'K') :: ([Char], Bool, Char)
--
-- (e) [(False,False),(True,False),(False,True),(True,True)]
-- R.: [(False,False),(True,False),(False,True),(True,True)] :: [(Bool, Bool)]
--
-- (f) filter id
-- R.:  filter :: (a -> Bool) -> [a] -> [a]
--      id :: b -> b => a = b, b = Bool => a = Bool
--      portanto, (filter id) :: [Bool] -> [Bool]
