
-- Trabalho final
-- Estoque de Produtos
--Provavelmente com bugs

data Produto = Vazio | Produto {nome::String, quant::Double, valor::Double} deriving (Eq, Ord, Show)

data Tree t = Null | Node (Tree t) t (Tree t) deriving (Eq, Ord, Show)

 -- Calcula o valor total dos produtos
calculaProd :: Tree Produto -> Double
calculaProd Null = 0
calculaProd (Node esq prod dir) = calculaProd esq + quant prod * valor prod + calculaProd dir

-- Busca e retorna o rpoduto
busca :: Tree Produto -> String -> IO ()
busca Null _ = putStrLn "Vazio"
busca (Node esq x dir) name
            |nome x == name = print x
            |nome x > name = busca esq name
            |otherwise = busca dir name

-- Busca e atualiza produto
buscaAtualiza :: Tree Produto -> Produto -> Tree Produto
buscaAtualiza Null prod = Node Null prod Null
buscaAtualiza (Node esq x dir) prod
        | nome x == nome prod = Node esq prod dir
        | nome prod > nome x =Node esq x (buscaAtualiza dir prod)
        | otherwise = Node (buscaAtualiza esq prod) x dir

--Imprimir em ordem
imprime :: Tree Produto  -> IO()
imprime Null = putStrLn ""
imprime (Node esq valor dir) = do
    imprime esq
    print valor
    imprime dir

-- remove os produtos com estoque zerado
vendeRemove :: Tree Produto -> Tree Produto
vendeRemove Null = Null
vendeRemove (Node esq x dir) 
   | quant x < 1 = removeProd (Node esq x dir) x
   |otherwise = Node (vendeRemove esq) x (vendeRemove dir)

-- vende os produtos e atualiza a quantidade
venderProd :: Tree Produto -> String -> Double -> Tree Produto
venderProd Null _ _ = Null
venderProd (Node esq x dir) nomeProd venda 
          | nome x == nomeProd && venda <= quant x = Node esq Produto{nome = nome x,quant = quant x - venda, valor = valor x} dir
          | nome x > nomeProd = Node esq x (venderProd dir nomeProd venda)
          |otherwise = Node (venderProd esq nomeProd venda) x dir

-- Funçao que remove os produtos

--Obs: pesquisei a logica, pois não tava conseguindo unir 
removeProd :: Tree Produto -> Produto -> Tree Produto
removeProd Null _ = Null
removeProd (Node esq prod dir) rProd
          |nome rProd < nome prod = Node (removeProd esq rProd) prod dir
          |nome rProd > nome prod = Node esq prod (removeProd dir rProd)
          |esq == Null = dir
          |dir == Null = esq
          |otherwise = unionTree esq dir
-- funcao que coloca o menor elemento da subarvore a direita e adiciona no lugar do removido
unionTree :: Tree Produto  -> Tree Produto -> Tree Produto
unionTree esq dir = Node esq menor novaDir
          where 
            menor = menorTree dir
            novaDir = removeProd dir menor
-- funcao que retorna o menor elemento a direita
menorTree :: Tree Produto -> Produto
menorTree Null = Vazio
menorTree (Node esq x dir)
          | esq == Null = x
          |otherwise = menorTree esq
         

menu :: Tree Produto -> IO ()
menu dados = do
  putStrLn "\n--------MENU--------"
  putStrLn "Digite 1 para Buscar um produto"
  putStrLn "Digite 2 para Atualizar / Adicionar produto"
  putStrLn "Digite 3 para o valor total dos produtos"
  putStrLn "Digite 4 vender algum produto"
  putStrLn "Digite 5 imprimir"
  putStrLn "Digite 0 para sair"
  putStr "Opção: "
  opt <- getChar
  getChar -- descarta o Enter
  case opt of
    --Busca o produto
    '1' -> do
      putStrLn "Digite o nome do produto"
      nomeBusca <- getLine
      busca dados nomeBusca
      menu dados
    -- Busca e Atualiza ou adiciona
    '2' -> do
        putStrLn "\n-----Atualizar / Adicionar produto----\n"
        putStrLn "Digite o nome do produto"
        nomeBusca <- getLine
        putStrLn "Digite a quantidade do produto"
        quantBusca <- getLine
        putStrLn "Digite o valor do produto"
        valorBusca <- getLine
        let db = buscaAtualiza dados Produto {nome = nomeBusca, quant = read quantBusca::Double, valor = read valorBusca::Double}
        imprime db
        menu db
    --Calcula o valor total dos produtos em estoque
    '3' -> do
      putStrLn ("R$" ++ show (calculaProd dados))
      menu dados
    --Vende o produto
    '4' -> do
        putStrLn "\n-----Vender produto----\n"
        putStrLn "Digite o nome do produto"
        nomeBusca <- getLine
        putStrLn "Digite a quantidade do produto"
        quantBusca <- getLine
        --Vende o produto e atualiza 
        let db = venderProd dados nomeBusca (read quantBusca::Double)
        let aux = vendeRemove db
        imprime aux
        menu aux
    '5' -> do
      imprime dados
      menu dados
    '0' ->
      putStrLn "\n--------FIM--------"
    _ -> do
      putStrLn "\nOpção inválida!"
      menu dados

main :: IO ()
main = do
  menu Null
  return ()






