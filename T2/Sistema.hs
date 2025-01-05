{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Main where

-- Importação de bibliotecas necessárias
import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import Text.Read (readMaybe)

-- Tipos personalizados para representar posições e espaços tridimensionais
type Posicao = (Int, Int, Int) -- Representa uma posição no espaço 3D (x,y,z)
type Espaco = (Posicao, Posicao) -- Representa um espaço definido por dois pontos (min, max)

-- Estrutura de dados que representa uma nave alienígena
data Nave = Nave
    { naveId :: String -- Identificador único da nave
    , posicao :: Posicao -- Posição atual (x,y,z)
    , ligado :: Bool -- Estado da nave (ligado/desligado)
    , espacoPermitido :: Espaco -- Espaço permitido para a nave se mover
    }
    deriving (Show, Eq)

-- Estrutura de dados que representa um registro de ações realizadas em uma nave
data Registo = Registo
    { comandosExecutados :: [(String, String, String, Posicao)] -- Lista de comandos: (ID, ação, parâmetros, posição)
    , estadoFinal :: Nave -- Estado final da nave após a execução dos comandos
    }

-- Verifica se duas posições são iguais no espaço 3D
mesmasPosicoes :: Posicao -> Posicao -> Bool
mesmasPosicoes (x1, y1, z1) (x2, y2, z2) = x1 == x2 && y1 == y2 && z1 == z2

-- Verifica se há colisão entre naves em uma determinada posição
verificarColisao :: [Nave] -> Nave -> Posicao -> Bool
verificarColisao todasNaves naveAtual novaPosicao =
    any
        ( \outraNave ->
            naveId outraNave /= naveId naveAtual
                && mesmasPosicoes novaPosicao (posicao outraNave)
        )
        todasNaves

-- Converte uma string de coordenadas "(x,y,z)" em um tipo Posicao
lerCoordenadas :: String -> Maybe Posicao
lerCoordenadas str = do
    let cleanStr = filter (`notElem` "()") str -- Remove parênteses
    let parts = words $ map (\c -> if c == ',' then ' ' else c) cleanStr -- Substitui vírgulas por espaços
    case parts of
        [x, y, z] -> case (readMaybe x, readMaybe y, readMaybe z) of
            (Just x', Just y', Just z') -> Just (x', y', z')
            _ -> Nothing
        _ -> Nothing

-- Converte uma Posicao em uma string formatada "(x,y,z)"
formatarPosicao :: Posicao -> String
formatarPosicao (x, y, z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

-- Função que lê o conteúdo do arquivo de texto e retorna uma lista de naves
parseNave :: String -> [Nave]
parseNave conteudo = foldl verificarColisoes [] (map parseLinha (lines conteudo))
  where
    posicaoPadrao = (0, 0, 0) -- Posição inicial padrão

    -- Processa uma única linha do arquivo de configuração
    parseLinha linha = case words linha of
        (id : comandos) -> processarComandos (Nave id posicaoPadrao False ((0, 0, 0), (100, 100, 100))) comandos
        _ -> Nave "ERRO" posicaoPadrao False ((0, 0, 0), (100, 100, 100))

    -- Verificar colisões e encontrar uma nova posição se necessário
    verificarColisoes :: [Nave] -> Nave -> [Nave]
    verificarColisoes naves naveAtual
        | any (mesmasPosicoes (posicao naveAtual) . posicao) naves =
            -- Se a posição da nave atual já está ocupada, encontrar uma nova posição
            let novaPos = encontrarProximaPosicaoLivre naves (posicao naveAtual)
             in naveAtual{posicao = novaPos} : naves
        | otherwise = naveAtual : naves

    -- Encontrar a próxima posição livre
    encontrarProximaPosicaoLivre :: [Nave] -> Posicao -> Posicao
    encontrarProximaPosicaoLivre naves (x, y, z) =
        if any (\nave -> mesmasPosicoes (x, y, z) (posicao nave)) naves
            then encontrarProximaPosicaoLivre naves (x + 1, y + 1, z + 1)
            else (x, y, z)

    -- Processa os comandos de inicialização da nave
    processarComandos :: Nave -> [String] -> Nave
    processarComandos nave [] = nave
    processarComandos nave (cmd : args) =
        let updatedNave = case cmd of
                "init" -> case args of
                    (pos : status : rest) ->
                        case lerCoordenadas pos of
                            Just posicaoInicial ->
                                processarComandos (nave{posicao = posicaoInicial, ligado = status == "1"}) rest
                            Nothing -> processarComandos nave rest
                    _ -> processarComandos nave args
                "initspace" -> case args of
                    (min : max : rest) ->
                        case (lerCoordenadas min, lerCoordenadas max) of
                            (Just minPos, Just maxPos) ->
                                processarComandos (nave{espacoPermitido = (minPos, maxPos)}) rest
                            _ -> processarComandos nave rest
                    _ -> processarComandos nave args
                "move" ->
                    let processMovements naveAtual [] = naveAtual
                        processMovements naveAtual (movimento : restoMovimentos) =
                            case lerCoordenadas movimento of
                                Just coords ->
                                    case validarMovimento [] naveAtual coords of
                                        Right naveNova -> processMovements naveNova restoMovimentos
                                        Left _ -> processMovements naveAtual restoMovimentos
                                Nothing -> processMovements naveAtual restoMovimentos
                     in processMovements nave args
                "acao" -> case args of
                    (acao : rest) ->
                        let acaoLimpa = filter (`notElem` "()") acao
                         in case acaoLimpa of
                                "ligar" -> nave{ligado = True}
                                "desligar" -> nave{ligado = False}
                                _ -> nave
                    _ -> nave
                _ -> nave
         in processarComandos updatedNave args

-- Valida se um movimento é permitido para uma nave
validarMovimento :: [Nave] -> Nave -> Posicao -> Either String Nave
validarMovimento todasNaves nave (dx, dy, dz) =
    if not (ligado nave)
        then Left "Erro: A nave precisa estar ligada para mover!"
        else do
            let (x, y, z) = posicao nave
                novaPosicao = (x + dx, y + dy, z + dz)
            if not (verificarEspacoPermitido novaPosicao nave)
                then Left "Erro: Movimento fora do espaço permitido!"
                else
                    if verificarColisao todasNaves nave novaPosicao
                        then Left "Erro: Movimento impossível - colisão detectada com outra nave!"
                        else Right nave{posicao = novaPosicao}

-- Verifica se uma posição está dentro do espaço permitido da nave
verificarEspacoPermitido :: Posicao -> Nave -> Bool
verificarEspacoPermitido (x, y, z) nave =
    let ((minX, minY, minZ), (maxX, maxY, maxZ)) = espacoPermitido nave
     in x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ

-- Liga uma nave se ela estiver desligada
ligarNave :: Nave -> Either String Nave
ligarNave nave =
    if ligado nave
        then Left "Erro: A nave já está ligada!"
        else Right nave{ligado = True}

-- Desliga uma nave se ela estiver ligada
desligarNave :: Nave -> Either String Nave
desligarNave nave =
    if not (ligado nave)
        then Left "Erro: A nave já está desligada!"
        else Right nave{ligado = False}

-- Grava o estado atual das naves em um arquivo de texto
gravarNavesFicheiro :: FilePath -> [Nave] -> IO ()
gravarNavesFicheiro caminho naves = do
    let conteudo = unlines $ map formatarNave naves
    writeFile caminho conteudo
  where
    formatarNave nave =
        naveId nave
            ++ " init "
            ++ formatarPosicao (posicao nave)
            ++ " "
            ++ (if ligado nave then "1" else "0")
            ++ " initspace "
            ++ formatarPosicao posMin
            ++ " "
            ++ formatarPosicao posMax
      where
        (posMin, posMax) = espacoPermitido nave

-- Exibe informações detalhadas de uma nave
mostrarNave :: Nave -> IO ()
mostrarNave nave = do
    putStrLn $ "\nNave ID: " ++ naveId nave
    putStrLn $ "Posição: " ++ show (posicao nave)
    putStrLn $ "Estado: " ++ if ligado nave then "Ligada" else "Desligada"
    putStrLn $ "Espaço permitido: " ++ show (espacoPermitido nave)
    putStrLn "----------------------------------------"

-- Exibe o menu principal e retorna a opção escolhida
menuPrincipal :: IO (Maybe Int)
menuPrincipal = do
    putStrLn "\n=== Sistema de Controle de Nave Alienígena ==="
    putStrLn "1. Listar todas as naves"
    putStrLn "2. Executar ações para uma nave"
    putStrLn "3. Executar ações para todas as naves"
    putStrLn "4. Entrada direta de comandos para uma ou mais naves"
    putStrLn "5. Sair"
    putStr "Escolha uma opção: "
    hFlush stdout
    input <- getLine
    return $ readMaybe input

-- Exibe o menu de ações para uma nave específica
menuAcoes :: [Nave] -> Nave -> Registo -> IO Registo
menuAcoes todasNaves nave registo = do
    putStrLn "\n=== Menu de Ações ==="
    putStrLn "1. Ligar nave"
    putStrLn "2. Desligar nave"
    putStrLn "3. Mover nave"
    putStrLn "4. Voltar"
    putStr "Escolha uma ação: "
    hFlush stdout
    opcao <- getLine
    let estadoAntes = nave
    case opcao of
        "1" -> processarAcao "Ligar nave" (ligarNave nave) estadoAntes
        "2" -> processarAcao "Desligar nave" (desligarNave nave) estadoAntes
        "3" -> do
            putStrLn "Digite o movimento (x y z):"
            input <- getLine
            case processarMovimento todasNaves input nave of
                Left erro -> do
                    putStrLn erro
                    menuAcoes todasNaves nave registo
                Right naveAtualizada -> do
                    putStrLn "Movimento realizado com sucesso!"
                    menuAcoes todasNaves naveAtualizada (atualizarRegisto registo "Mover nave" input estadoAntes naveAtualizada)
        "4" -> do
            mostrarRegistro registo
            return registo
        _ -> do
            putStrLn "Opção inválida!"
            menuAcoes todasNaves nave registo
  where
    -- Processa uma ação genérica (ligar/desligar)
    processarAcao acao result estadoAntes = case result of
        Left erro -> do
            putStrLn erro
            menuAcoes todasNaves nave registo
        Right naveAtualizada -> do
            putStrLn "Ação realizada com sucesso!"
            menuAcoes todasNaves naveAtualizada (atualizarRegisto registo acao "" estadoAntes naveAtualizada)

    -- Atualiza o registro de comandos
    atualizarRegisto reg acao params antes depois =
        reg{comandosExecutados = (naveId antes, acao, params, posicao depois) : comandosExecutados reg, estadoFinal = depois}

    mostrarRegistro reg = do
        putStrLn "\nComandos Executados:"
        mapM_
            ( \(idNave, acao, params, estado) -> do
                putStrLn $ "ID: " ++ idNave ++ ", Ação: " ++ acao ++ ", Parâmetros: " ++ params ++ ", Posição: " ++ show estado
            )
            (reverse $ comandosExecutados reg)

-- Processa um movimento com base na entrada do usuário
processarMovimento :: [Nave] -> String -> Nave -> Either String Nave
processarMovimento todasNaves input nave = do
    let coords = words input
    case mapM readMaybe coords of
        Just [x, y, z] -> validarMovimento todasNaves nave (x, y, z)
        _ -> Left "Coordenadas inválidas! Use o formato: x y z"

-- Executa ações para uma nave específica
executarAcoesUmaNave :: [Nave] -> IO [Nave]
executarAcoesUmaNave naves = do
    putStr "Digite o ID da nave: "
    hFlush stdout
    id <- getLine
    case find (\n -> naveId n == id) naves of
        Nothing -> do
            putStrLn "Nave não encontrada!"
            return naves
        Just nave -> do
            let registroInicial = Registo [] nave
            registroAtualizado <- menuAcoes naves nave registroInicial
            return $ map (\n -> if naveId n == id then estadoFinal registroAtualizado else n) naves

-- Executa ações para todas as naves sequencialmente
executarAcoesTodasNaves :: [Nave] -> IO [Nave]
executarAcoesTodasNaves naves = do
    putStrLn "\nExecutando ações para todas as naves..."
    foldM
        ( \navesAtuais nave -> do
            putStrLn $ "\nExecutando ações para nave " ++ naveId nave
            let registroInicial = Registo [] nave
            registroAtualizado <- menuAcoes navesAtuais nave registroInicial
            return $ map (\n -> if naveId n == naveId nave then estadoFinal registroAtualizado else n) navesAtuais
        )
        naves
        naves

-- Entrada direta de comandos para uma ou mais naves
entradaDireta :: [Nave] -> IO [Nave]
entradaDireta naves = do
    putStrLn "\nDigite o comando no formato: ID COMANDO PARAMETROS"
    putStrLn "Exemplo: nave1 acao ligar"
    putStrLn "Exemplo: nave1 mover 10 20 30"
    comandosIntroduzidos <- loopEntrada naves []
    putStrLn "\nInstruções introduzidas:"
    mapM_ (putStrLn . formatarComando) comandosIntroduzidos
    let navesAtualizadas = executarComandos naves comandosIntroduzidos
    return navesAtualizadas
  where
    -- Loop para entrada de comandos
    loopEntrada :: [Nave] -> [(String, String, String, Posicao)] -> IO [(String, String, String, Posicao)]
    loopEntrada naves comandos = do
        putStr "Introduza uma instrução para uma nave ou escreva 'voltar' para voltar ao menu: "
        hFlush stdout
        input <- getLine

        -- Verifica se o usuário deseja voltar ao menu principal
        if input == "voltar"
            then return comandos
            else do
                let palavras = words input
                case palavras of
                    (id : cmd : params) ->
                        case find (\n -> naveId n == id) naves of
                            Nothing -> do
                                putStrLn "Nave não encontrada!"
                                loopEntrada naves comandos
                            Just nave -> processarComando nave cmd params comandos
                    _ -> do
                        putStrLn "Formato de comando inválido!"
                        loopEntrada naves comandos

    -- Processa um comando introduzido pelo usuário
    processarComando :: Nave -> String -> [String] -> [(String, String, String, Posicao)] -> IO [(String, String, String, Posicao)]
    processarComando nave cmd params comandos = case cmd of
        "mover" -> case params of
            [x, y, z] -> case (readMaybe x, readMaybe y, readMaybe z) of
                (Just dx, Just dy, Just dz) -> do
                    case validarMovimento naves nave (dx, dy, dz) of
                        Left erro -> do
                            putStrLn erro
                            loopEntrada naves comandos
                        Right naveAtualizada -> do
                            let novoComando = (naveId nave, "mover", show (dx, dy, dz), posicao naveAtualizada)
                            loopEntrada naves (novoComando : comandos)
                _ -> do
                    putStrLn "Parâmetros de movimento inválidos!"
                    loopEntrada naves comandos
            _ -> do
                putStrLn "Parâmetros de movimento inválidos!"
                loopEntrada naves comandos
        "acao" -> case params of
            ["ligar"] -> do
                case ligarNave nave of
                    Left erro -> do
                        putStrLn erro
                        loopEntrada naves comandos
                    Right naveAtualizada -> do
                        let novoComando = (naveId nave, "acao", "ligar", posicao naveAtualizada)
                        loopEntrada naves comandos
            ["desligar"] -> do
                case desligarNave nave of
                    Left erro -> do
                        putStrLn erro
                        loopEntrada naves comandos
                    Right naveAtualizada -> do
                        let novoComando = (naveId nave, "acao", "desligar", posicao naveAtualizada)
                        loopEntrada naves (novoComando : comandos)
            _ -> do
                putStrLn "Formato de comando inválido para ação!"
                loopEntrada naves comandos
        _ -> do
            putStrLn "Comando desconhecido!"
            loopEntrada naves comandos

    formatarComando (idNave, acao, params, estado) =
        idNave ++ " " ++ acao ++ " " ++ params ++ " posição " ++ show estado

    -- Executa uma lista de comandos em uma lista de naves
    executarComandos :: [Nave] -> [(String, String, String, Posicao)] -> [Nave]
    executarComandos naves comandos =
        foldl
            ( \navesAtuais (id, cmd, params, _) ->
                case find (\n -> naveId n == id) navesAtuais of
                    Nothing -> navesAtuais
                    Just nave -> case cmd of
                        "mover" -> case lerCoordenadas params of
                            Just coords -> case validarMovimento navesAtuais nave coords of
                                Right naveAtualizada -> atualizarNave navesAtuais naveAtualizada
                                Left _ -> navesAtuais
                            Nothing -> navesAtuais
                        "acao" -> case params of
                            "ligar" -> case ligarNave nave of
                                Right naveAtualizada -> atualizarNave navesAtuais naveAtualizada
                                Left _ -> navesAtuais
                            "desligar" -> case desligarNave nave of
                                Right naveAtualizada -> atualizarNave navesAtuais naveAtualizada
                                Left _ -> navesAtuais
                            _ -> navesAtuais
                        _ -> navesAtuais
            )
            naves
            comandos

    -- Atualiza uma nave na lista de naves
    atualizarNave :: [Nave] -> Nave -> [Nave]
    atualizarNave naves naveAtualizada =
        map (\n -> if naveId n == naveId naveAtualizada then naveAtualizada else n) naves

-- Loop principal do programa
loopPrincipal :: [Nave] -> IO ()
loopPrincipal naves = do
    opcaoMaybe <- menuPrincipal
    case opcaoMaybe of
        Nothing -> do
            putStrLn "Entrada inválida! Por favor digite um número."
            loopPrincipal naves
        Just opcao -> case opcao of
            1 -> do
                putStrLn "\nListando todas as naves:"
                mapM_ mostrarNave naves
                loopPrincipal naves
            2 -> do
                navesAtualizadas <- executarAcoesUmaNave naves
                loopPrincipal navesAtualizadas
            3 -> do
                navesAtualizadas <- executarAcoesTodasNaves naves
                loopPrincipal navesAtualizadas
            4 -> do
                navesAtualizadas <- entradaDireta naves
                loopPrincipal navesAtualizadas
            5 -> do
                putStrLn "\nSalvando estado das naves..."
                gravarNavesFicheiro "AlienshipSave.txt" naves
                putStrLn "Estado salvo com sucesso!"
                putStrLn "Encerrando o programa..."
            _ -> do
                putStrLn "Opção inválida! Escolha entre 1 e 5."
                loopPrincipal naves

-- Função principal que carrega o arquivo de naves e inicia o loop principal
main :: IO ()
main = do
    putStrLn "A carregar arquivo de naves..."
    conteudo <- readFile "Alienship.txt"
    let naves = parseNave conteudo
    putStrLn "Arquivo carregado com sucesso!"
    loopPrincipal naves