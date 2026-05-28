-- Em Haskell nao existe "rodar linha por linha" como em Python ou Ruby
-- o programa inteiro é avaliado como expressões matemáticas
-- tudo que nao é IO fica aqui fora, puro e imutável


-- IMUTABILIDADE
-- isso nao é uma variável, é uma definição
-- nomeRepositorio nunca vai mudar, ele simplesmente "é" esse valor
-- nao existe nomeRepositorio = nomeRepositorio ++ "!" em Haskell
nomeRepositorio :: String
nomeRepositorio = "Livraria Funcional"


-- TIPO PERSONALIZADO com data
-- data cria um novo tipo, parecido com uma classe em OO mas imutável por natureza
-- Livro é o nome do tipo
-- Livro String String Int é o construtor de dados — os campos sao: titulo, autor, ano
-- uma vez criado um Livro, seus valores nao mudam nunca
data Livro = Livro String String Int


-- FUNÇÃO PURA
-- a assinatura "Livro -> String" diz: recebe um Livro e devolve uma String
-- essa função nao acessa nada de fora, só o argumento l
-- mesma entrada = mesma saída, sempre
descreverLivro :: Livro -> String
descreverLivro l =
    -- PATTERN MATCHING no construtor
    -- (Livro titulo autor ano) "desestrutura" o objeto, extraindo os campos pelo nome
    -- parecido com desestruturação em JavaScript ou unpacking em Python
    let (Livro titulo autor ano) = l
    -- show converte o Int para String, necessário para concatenar com ++
    in titulo ++ " - " ++ autor ++ " (" ++ show ano ++ ")"


-- GUARDS
-- guards sao como if/else mas declarativos — cada condição fica numa linha com |
-- o programa executa o primeiro caso verdadeiro que encontrar
-- otherwise é o "else" do Haskell, equivale ao True e sempre é o último caso
classificarLivro :: Livro -> String
classificarLivro livro
    | ano >= 2020 = titulo ++ " é um lançamento recente."
    | ano >= 2000 = titulo ++ " é do século XXI."
    | otherwise   = titulo ++ " é um clássico."
    -- let dentro de guards extrai os campos do construtor para usar nas condições
    where (Livro titulo _ ano) = livro
          -- _ ignora o campo autor, que nao precisamos aqui


-- FUNÇÕES DE ALTA ORDEM — map
-- map recebe uma função e uma lista, aplica a função em cada elemento
-- e devolve uma nova lista com os resultados — a original nunca é tocada
-- a assinatura "[Livro] -> [String]" diz: recebe lista de Livro, devolve lista de String
descreverTodos :: [Livro] -> [String]
descreverTodos livros = map descreverLivro livros
-- map descreverLivro é uma aplicação parcial:
-- estamos passando só o primeiro argumento de map, criando uma nova função


-- FUNÇÕES DE ALTA ORDEM — filter
-- filter recebe uma condição (função que retorna Bool) e uma lista
-- devolve só os elementos que passam na condição
livrosRecentes :: [Livro] -> [Livro]
livrosRecentes livros = filter ehRecente livros
    where
        -- ehRecente é uma função auxiliar local definida com where
        -- ela recebe um Livro e devolve True ou False
        ehRecente :: Livro -> Bool
        ehRecente (Livro _ _ ano) = ano >= 2000


-- FUNÇÕES DE ALTA ORDEM — sum e map juntos
-- map (\(Livro _ _ ano) -> ano) extrai os anos de todos os livros numa lista de Int
-- sum reduz essa lista a um único valor somando tudo
somaAnos :: [Livro] -> Int
somaAnos livros = sum (map (\(Livro _ _ ano) -> ano) livros)
-- \ é a sintaxe de lambda em Haskell — equivale ao lambda do Python ou ao => do JavaScript
-- \(Livro _ _ ano) -> ano significa: "dado um Livro, devolve o ano dele"


-- PATTERN MATCHING em listas
-- em vez de acessar por índice como lista[0], descrevemos o formato esperado
-- [] casa com lista vazia
-- (x:_) separa o primeiro elemento x do resto (que ignoramos com _)
-- (x:xs) separaria o primeiro elemento x do restante xs
primeiroPorAno :: [Livro] -> String
primeiroPorAno [] = "Nenhum livro encontrado."
primeiroPorAno (x:_) = "Primeiro livro: " ++ descreverLivro x


-- IO — EFEITOS COLATERAIS
-- tudo acima é puro, nao toca o mundo externo
-- IO é o tipo que marca funções que causam efeitos colaterais (ler, imprimir, etc.)
-- main é o único ponto de entrada do programa e o único lugar onde IO pode rodar
main :: IO ()
-- do permite encadear várias ações IO em sequência, como um bloco de comandos
main = do
    -- putStrLn imprime uma String com quebra de linha no final
    putStrLn "\n--- Livraria Funcional ---"
    putStrLn $ "Repositório: " ++ nomeRepositorio

    -- LISTA DE LIVROS
    -- em Haskell listas sao definidas com [] e elementos separados por vírgula
    -- todos os elementos precisam ser do mesmo tipo — aqui todos sao Livro
    let livros = [ Livro "Witch Hat Atelier Vol.1 (Grimoire Edition)" "Kamome Shirahama" 2023
                 , Livro "Sousou no Frieren Vol.1"                    "Kanehito Yamada"  2022
                 , Livro "Call of Cthulhu"                            "H.P. Lovecraft"   1928
                 , Livro "Nao Me Abandone Jamais"                     "Kazuo Ishiguro"   1989
                 , Livro "O Encanamento que Geme"                     "Junji Ito"        2023
                 ]

    -- mapM_ é o map para IO — aplica uma ação IO em cada elemento da lista
    -- putStrLn é a ação aplicada em cada String gerada por descreverLivro
    putStrLn "\nTodos os livros:"
    mapM_ putStrLn (descreverTodos livros)

    putStrLn "\nClassificacao:"
    -- mapM_ com uma lambda — para cada livro, imprime a classificação
    mapM_ (\l -> putStrLn (classificarLivro l)) livros

    putStrLn "\nLivros recentes (ano >= 2000):"
    mapM_ putStrLn (descreverTodos (livrosRecentes livros))

    -- show converte qualquer valor showable em String para poder concatenar
    putStrLn $ "\nSoma dos anos de publicacao: " ++ show (somaAnos livros)

    putStrLn $ "\n" ++ primeiroPorAno livros