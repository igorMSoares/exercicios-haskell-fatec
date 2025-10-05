-- 5.1) Crie o tipo TipoProduto que possui os values constructors
-- Escritorio , Informatica , Livro , Filme e Total .
--
-- O tipo Produto possui um value constructor - de mesmo nome -
-- e os campos valor ( Double ), tp ( TipoProduto )
-- e um value constructor Nada , que representa a ausência de um Produto .
--
-- Deseja-se calcular o valor total de uma compra, de modo a não
-- ter nenhuma conversão para inteiro e de forma combinável.
-- Crie uma instância de monoide para Produto , de modo que o retorno
-- sempre tenha Total no campo tp e a soma dos dois produtos em valor .
--
-- Explique como seria o exercício sem o uso de monoides.
-- Qual(is) seria(m) a(s) diferença(s)?
--
-- R.: Sem o uso de monoídes, seria necessário criar funções específicas para o tipo Produto, definindo como percorrer uma lista de produtos combinando-os em um Produto do tipo Total e tratando explicitamente o caso de lista vazia. Definindo uma instância de Monoid para Produto, basta utilizar o método mconcat presente em qualquer monóide, além do benefício de poder aproveitar as outras diversas funções disponíveis para monóides.
data TipoProduto = Escritorio | Informatica | Livro | Filme | Total deriving (Show)

data Produto = Produto {valor :: Double, tp :: TipoProduto} | Nada deriving (Show)

instance Semigroup Produto where
  Nada <> p = p
  p <> Nada = p
  p1 <> p2 = Produto {valor = v, tp = Total}
    where
      v = valor p1 + valor p2

instance Monoid Produto where
  mempty = Nada

-- 5.2) Crie uma função totalGeral que
-- recebe uma lista de produtos e
-- retorna o preço total deles usando o monoide anterior.
totalGeral :: [Produto] -> Produto
totalGeral = mconcat

-- 5.3) A função min no Haskell retorna o menor entre dois números,
-- por exemplo, min 4 5 = 4 .
-- Crie um tipo Min com um campo inteiro, que seja
-- instância de Ord , Eq e Show (deriving).
-- Crie uma instância de Monoid para Min
-- ( maxBound representa o maior inteiro existente no Haskell).
--
-- Quanto vale a expressão Min (-32) <> Min (-34) <> Min (-33) ?
-- Explique sua escolha para o mempty .
--
-- R.: Min (-32) <> Min (-34) <> Min (-33) = Min (-34)
--     (mempty = Min maxBound) pois (Min x <> Min maxBound = Min x) para todo x :: Int, visto que (<>) utiliza a função (min) e (min x maxBound = x) para todo x :: Int
data Min = Min Int deriving (Ord, Eq, Show)

instance Semigroup Min where
  Min x <> Min y = Min (min x y)

instance Monoid Min where
  mempty = Min maxBound

-- 5.4) Crie uma função minAll que recebe um [Min] e
-- retorna um Min contendo o menor valor.
minAll :: [Min] -> Min
minAll = mconcat

-- 5.5) Crie o tipo Paridade com os values constructors Par e Impar .
-- Crie o typeclass ParImpar que contém a função
-- decide :: a -> Paridade
-- e possui as instâncias:
-- - Para Int : noção de Par/Impar de Int .
-- - Para [a] : uma lista de elementos qualquer é Par se o número de elementos o for.
-- - Bool : False como Par , True como Impar .
data Paridade = Par | Impar deriving (Show)

class ParImpar a where
  decide :: a -> Paridade

instance ParImpar Int where
  decide x
    | even x = Par
    | otherwise = Impar

instance ParImpar [a] where
  decide xs
    | even $ length xs = Par
    | otherwise = Impar

instance ParImpar Bool where
  decide False = Par
  decide True = Impar

-- 5.6) A função max no Haskell retorna
-- o maior entre dois números,
-- por exemplo: max 4 5 = 5 .
--
-- Crie um tipo Max com um campo inteiro que seja
-- instância de Ord , Eq e Show (deriving).
--
-- Crie uma instância de Monoid para Max
-- ( minBound representa o menor inteiro existente no Haskell ).
-- Quanto vale a expressão Max 10 <> Max 13 <> Max 5?
-- Explique sua escolha para o mempty .
--
-- Crie uma função maxAll que recebe um [Max] e
-- retorna um Max contendo o maior valor.
--
-- R.: Max 10 <> Max 13 <> Max 5 = Max 13
--     (mempty = Max minBound) pois (Max x <> Max minBound = Max x) para todo x :: Int, visto que (<>) utiliza a função (max) e (max x minBound = x) para todo x :: Int
data Max = Max Int deriving (Ord, Eq, Show)

instance Semigroup Max where
  Max x <> Max y = Max (max x y)

instance Monoid Max where
  mempty = Max minBound

maxAll :: [Max] -> Max
maxAll = mconcat

-- 5.7) Usando a estrutura de árvore, monte uma função mapa ,
-- que jogue uma função passada por parâmetro para todos os
-- elementos de uma árvore. Deixe explícito o tipo desta função.
data Tree a = Leaf a | Branch a (Tree a) (Tree a) | Empty deriving (Show)

mapa :: (Int -> Int) -> Tree Int -> Tree Int
mapa f (Leaf x) = Leaf (f x)
mapa f (Branch x l r) = Branch (f x) (mapa f l) (mapa f r)
mapa f Empty = Empty

-- 5.8) Usando o exercício anterior,
-- some 5 a cada elemento de uma árvore de inteiros.
--
-- R.:
-- ghci> mapa (+5) (Branch 2 (Leaf 3) (Branch 1 Empty (Leaf 10)))
-- Branch 7 (Leaf 8) (Branch 6 Empty (Leaf 15))

-- 5.9) Uma lista ordenada é uma lista cujos elementos
-- são inseridos de forma ordenada (crescente).
-- Usando o tipo ListaOrd a = a :?: (ListaOrd a) | Nulo deriving Show ,
-- crie as funções:
-- - inserir :: (Ord a) => a -> ListaOrd a -> ListaOrd a
-- - remover :: (Eq a) => a -> ListaOrd a -> ListaOrd a
-- - tamanho :: ListaOrd a -> Int
--
-- Observação: a função remover deve buscar um elemento.
-- Se não achar, a lista deve se manter intacta.
data ListaOrd a = a :?: (ListaOrd a) | Nulo deriving (Show)

inserir :: Int -> ListaOrd Int -> ListaOrd Int
inserir x Nulo = x :?: Nulo
inserir x (n :?: rest)
  | x <= n = x :?: (n :?: rest)
  | otherwise = n :?: inserir x rest

remover :: Int -> ListaOrd Int -> ListaOrd Int
remover _ Nulo = Nulo
remover x (n :?: rest)
  | x > n = n :?: remover x rest
  | x < n = n :?: rest
  | x == n = rest

tamanho :: ListaOrd Int -> Int
tamanho Nulo = 0
tamanho (_ :?: rest) = 1 + tamanho rest

-- 5.10) Usando a estrutura de árvore vista,
-- faça uma função que some todos os elementos
-- de uma árvore de números.
treeSum :: Tree Int -> Int
treeSum Empty = 0
treeSum (Leaf x) = x
treeSum (Branch x l r) = x + treeSum l + treeSum r

-- 5.11) Implemente os percursos pós-ordem e pré-ordem.
-- Via comentário, faça os "testes de mesa" para os dois percursos
-- da árvore Raiz 15 (Raiz 11 (Folha 6) (Raiz 12 (Folha 10) Nula)) (Raiz 20 Nula (Raiz 22 (Folha 21) Nula)) .
postOrder :: Tree Int -> [Int]
postOrder Empty = []
postOrder (Leaf x) = [x]
postOrder (Branch x l r) = postOrder l ++ postOrder r ++ [x]

-- TESTE DE MESA --
-- postOrder (Branch 15 (Branch 11 (Leaf 6) (Branch 12 (Leaf 10) Empty)) (Branch 20 Empty (Branch 22 (Leaf 21) Empty)))
-- => postOrder (Branch 11 (Leaf 6) (Branch 12 (Leaf 10) Empty)) ++ postOrder (Branch 20 Empty (Branch 22 (Leaf 21) Empty)) ++ [15]
-- => (postOrder ((Leaf 6) (Branch 12 (Leaf 10) Empty)) ++ [11]) ++ (postOrder (Empty (Branch 22 (Leaf 21) Empty)) ++ [20]) ++ [15]
-- => (([6] ++ (postOrder (Branch 12 (Leaf 10) Empty)) ++ [11])) ++ ([] ++ (postOrder (Branch 22 (Leaf 21) Empty)) ++ [20]) ++ [15]
-- => (([6] ++ ((postOrder ((Leaf 10) Empty) ++ [12])) ++ [11])) ++ ([] ++ (((postOrder (Leaf 21) Empty) ++ [22])) ++ [20]) ++ [15]
-- => (([6] ++ ((([10] ++ (PostOrder Empty) ++ [12]))) ++ [11])) ++ ([] ++ ((([21] ++ (postOrder Empty)) ++ [22])) ++ [20]) ++ [15]
-- => (([6] ++ ((([10] ++ [] ++ [12]))) ++ [11])) ++ ([] ++ ((([21] ++ []) ++ [22])) ++ [20]) ++ [15]
-- => ([6] ++ [10, 12] ++ [11]) ++ ([] ++ ([21] ++ [22]) ++ [20]) ++ [15]
-- => [6, 10, 12, 11] ++ ([] ++ [21, 22] ++ [20]) ++ [15]
-- => [6, 10, 12, 11] ++ [21, 22, 20] ++ [15]
-- => [6, 10, 12, 11, 21, 22, 20, 15]

preOrder :: Tree Int -> [Int]
preOrder Empty = []
preOrder (Leaf x) = [x]
preOrder (Branch x l r) = [x] ++ preOrder l ++ preOrder r

inOrder :: Tree Int -> [Int]
inOrder Empty = []
inOrder (Leaf x) = [x]
inOrder (Branch x l r) = inOrder l ++ [x] ++ inOrder r

-- 5.12) Faça uma função para inserir um elemento em uma
-- árvore de busca binária (use a mesma estrutura vista).
bstInsert :: Int -> Tree Int -> Tree Int
bstInsert x Empty = Leaf x
bstInsert x (Leaf y)
  | x <= y = Branch y (Leaf x) Empty
  | x > y = Branch y Empty (Leaf x)
bstInsert x (Branch y l r)
  | x <= y = Branch y (bstInsert x l) r
  | x > y = Branch y l (bstInsert x r)

-- 5.13) Faça uma função que, a partir de uma lista de elementos de tipo,
-- insira todos os elementos desta lista na árvore e retorne-a,
-- usando o exercício anterior.
bstInsertAll :: [Int] -> Tree Int -> Tree Int
bstInsertAll [] xs = xs
bstInsertAll (x : xs) tree = bstInsertAll xs (bstInsert x tree)
