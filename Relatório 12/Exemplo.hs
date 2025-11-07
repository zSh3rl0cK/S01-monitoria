-- =========================================================================
-- Simulação de uma compra em uma livraria usando Haskell
-- Demonstra tipos personalizados, funções puras, listas e guards
-- =========================================================================

-- ** 1. Tipos Personalizados (data) **

-- Tipo que representa um livro
data Livro = Livro { titulo :: String, autor :: String, preco :: Double }
    deriving (Show) -- permite exibir o valor no terminal

-- Tipo enumerado com possíveis status de uma compra
data StatusCompra = Aberta | Finalizada | Devolvida
    deriving (Show, Eq) -- Eq permite comparar com ==

-- Tipo que representa uma compra com lista de livros e status
data Compra = Compra { itensComprados :: [Livro], statusCompra :: StatusCompra }
    deriving (Show)

-- ** 2. Funções Puras **

-- Retorna o preço de um livro (função pura)
precoLivro :: Livro -> Double
precoLivro (Livro _ _ p) = p  -- usa pattern matching e ignora campos do livro com _ (salva apenas preco)

-- Soma todos os preços de uma lista de livros
calculaSubtotal :: [Livro] -> Double
calculaSubtotal listaDeLivros = sum (map precoLivro listaDeLivros)
-- map aplica precoLivro em cada item, e sum soma os resultados

-- **3. Condicional com Guards **

-- Define o valor do frete
adicionaFrete :: Double -> Double
adicionaFrete subtotal
    | subtotal > 100.0  = 0.0   -- frete grátis se maior que 100
    | otherwise         = 12.0  -- caso contrário, frete padrão

-- ** 4. Função principal de cálculo do valor total **

valorTotalCompra :: Compra -> Double
valorTotalCompra compra
    | statusCompra compra == Devolvida = 0.0 -- devolvida: valor zero
    | otherwise = total
    where
        sub = calculaSubtotal (itensComprados compra) -- soma dos preços
        frete = adicionaFrete sub                     -- calcula o frete
        total = sub + frete                           -- total final

-- ** 5. Uso de Pattern Matching em Listas **

-- Retorna o título do primeiro livro da compra
primeiroTitulo :: Compra -> String
primeiroTitulo compra =
    case itensComprados compra of
        []    -> "Nenhum livro na compra."  -- lista vazia
        (x:_) -> titulo x                  -- (x:_) é o padrão de correspondência de lista (“cabeça e cauda”): 
                                            -- pega o título do primeiro livro

-- ** 6. Exemplos de dados **

livroA = Livro "Neuromancer" "William Gibson" 45.0
livroB = Livro "Call of Cthulhu" "H.P. Lovecraft" 65.0
livroC = Livro "O Corvo" "Edgar Alan Poe" 30.0

-- Cria três compras diferentes
compra1 = Compra [livroA, livroB] Aberta      -- frete grátis
compra2 = Compra [livroA, livroC] Aberta      -- com frete
compra3 = Compra [livroA, livroB] Devolvida   -- devolvida

-- ** 7. Função Principal (IO) **

main :: IO ()
main = do
    putStrLn "--- Simulação de Compras na Livraria ---"

    -- Exibe o valor total das compras
    putStr "Compra 1 (Frete Grátis) - Valor Total: R$ "
    print (valorTotalCompra compra1)

    putStr "Compra 2 (Com Frete) - Valor Total: R$ "
    print (valorTotalCompra compra2)

    putStr "Compra 3 (Devolvida) - Valor Total: R$ "
    print (valorTotalCompra compra3)

    -- Mostra o primeiro livro da compra 2
    putStrLn "\nPrimeiro livro na Compra 2: "
    putStrLn (primeiroTitulo compra2)
