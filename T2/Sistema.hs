{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Main where

import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import Text.Read (readMaybe)

-- Tipos básicos para representar posições de naves e espaço permitido
type Posicao = (Int, Int, Int)
type Espaco = (Posicao, Posicao)

-- Estrutura de dados principal para representar uma nave
data Nave = Nave
    { naveId :: String -- Identificador único da nave
    , posicao :: Posicao -- Posição atual (x, y, z)
    , ligado :: Bool -- Estado de energia (ligado/desligado)
    , espacoPermitido :: Espaco -- Espaço de movimento permitido ((minX,minY,minZ), (maxX,maxY,maxZ))
    }
    deriving (Show, Eq)

-- Estrutura para registrar comandos executados
data Registro = Registro
    { comandosExecutados :: [(String, String, String, Posicao)] -- Lista de (ID da nave, comando, parâmetros, posição)
    , estadoFinal :: Nave -- Estado final da nave
    }

-- Parse das coordenadas do formato de string "(x,y,z)"
lerCoordenadas :: String -> Maybe Posicao
lerCoordenadas str = do
    let cleanStr = filter (`notElem` "()") str
    let parts = words $ map (\c -> if c == ',' then ' ' else c) cleanStr
    case parts of
        [x, y, z] -> case (readMaybe x, readMaybe y, readMaybe z) of
            (Just x', Just y', Just z') -> Just (x', y', z')
            _ -> Nothing
        _ -> Nothing

-- Formatar uma posição como string
formatarPosicao :: Posicao -> String
formatarPosicao (x, y, z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

-- Parse das informações da nave a partir do conteúdo do arquivo
parseNave :: String -> [Nave]
parseNave conteudo = map parseLinha (lines conteudo)
  where
    parseLinha linha = case words linha of
        (id : comandos) -> processarComandos (Nave id (0, 0, 0) False ((0, 0, 0), (100, 100, 100))) comandos
        _ -> Nave "ERRO" (0, 0, 0) False ((0, 0, 0), (100, 100, 100))

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
                    let summedCoords =
                            foldl'
                                ( \(sx, sy, sz) c -> case lerCoordenadas c of
                                    Just (x, y, z) -> (sx + x, sy + y, sz + z)
                                    Nothing -> (sx, sy, sz)
                                )
                                (0, 0, 0 )
                                args
                     in case validarMovimento nave summedCoords of
                            Right naveAtualizada -> naveAtualizada
                            Left _ -> nave -- Retorna o estado atual da nave em caso de erro
                "acao" -> case args of
                    (acao : rest) ->
                        let acaoLimpa = filter (/= ' ') acao -- Remove espaços adicionais
                         in case acaoLimpa of
                                "ligar" -> nave { ligado = True }
                                "desligar" -> nave { ligado = False }
                                _ -> nave
                    _ -> nave -- Não há ação válida
                _ -> nave -- Para comandos desconhecidos, retornar o estado atual
         in processarComandos updatedNave args

-- Funções de validação de movimento
validarMovimento :: Nave -> Posicao -> Either String Nave
validarMovimento nave (dx, dy, dz) =
    if not (ligado nave)
        then Left "Erro: A nave precisa estar ligada para mover!"
        else
            let (x, y, z) = posicao nave
                novaPosicao = (x + dx, y + dy, z + dz)
             in if verificarEspacoPermitido novaPosicao nave
                    then Right nave { posicao = novaPosicao }
                    else Left "Erro: Movimento fora do espaço permitido!"

verificarEspacoPermitido :: Posicao -> Nave -> Bool
verificarEspacoPermitido (x, y, z) nave =
    let ((minX, minY, minZ), (maxX, maxY, maxZ)) = espacoPermitido nave
     in x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ

-- Controle de nave
ligarNave :: Nave -> Either String Nave
ligarNave nave =
    if ligado nave
        then Left "Erro: A nave já está ligada!"
        else Right nave { ligado = True }

desligarNave :: Nave -> Either String Nave
desligarNave nave =
    if not (ligado nave)
        then Left "Erro: A nave já está desligada!"
        else Right nave { ligado = False }

-- Gravar estados das naves em ficheiro
gravarNavesFicheiro :: FilePath -> [Nave] -> IO ()
gravarNavesFicheiro caminho naves = do
    let conteudo = unlines $ map formatarNave naves
    writeFile caminho conteudo
  where
    formatarNave nave =
        naveId nave
            ++ " init "
            ++ "("
            ++ formatarPosicao (posicao nave)
            ++ ") "
            ++ (if ligado nave then "1" else "0")
            ++ " initspace "
            ++ "("
            ++ formatarPosicao posMin
            ++ ") "
            ++ "("
            ++ formatarPosicao posMax
            ++ ")"
      where
        (posMin, posMax) = espacoPermitido nave

-- Mostrar informações da nave
mostrarNave :: Nave -> IO ()
mostrarNave nave = do
    putStrLn $ "\nNave ID: " ++ naveId nave
    putStrLn $ "Posição: " ++ show (posicao nave)
    putStrLn $ "Estado: " ++ if ligado nave then "Ligada" else "Desligada"
    putStrLn $ "Espaço permitido: " ++ show (espacoPermitido nave)
    putStrLn "----------------------------------------"

-- Menu principal
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

-- Menu de ações
menuAcoes :: Nave -> Registro -> IO Registro
menuAcoes nave registro = do
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
            case processarMovimento input nave of
                Left erro -> do
                    putStrLn erro
                    menuAcoes nave registro
                Right naveAtualizada -> do
                    putStrLn "Movimento realizado com sucesso!"
                    -- Atualiza o registro apenas se o movimento for bem-sucedido
                    menuAcoes naveAtualizada (atualizarRegistro registro "Mover nave" input estadoAntes naveAtualizada)  -- Continua no menu de ações
        "4" -> do
            mostrarRegistro registro  -- Mostra o registro de comandos
            return registro
        _ -> do
            putStrLn "Opção inválida!"
            menuAcoes nave registro  -- Retorna ao menu de ações
  where
    processarAcao acao result estadoAntes = case result of
        Left erro -> do
            putStrLn erro
            menuAcoes nave registro  -- Retorna ao menu de ações
        Right naveAtualizada -> do
            putStrLn "Ação realizada com sucesso!"
            -- Atualiza o registro apenas se a ação for bem-sucedida
            menuAcoes naveAtualizada (atualizarRegistro registro acao "" estadoAntes naveAtualizada)  -- Continua no menu de ações

    atualizarRegistro reg acao params antes depois =
        reg { comandosExecutados = (naveId antes, acao, params, posicao depois) : comandosExecutados reg, estadoFinal = depois }

    mostrarRegistro reg = do
        putStrLn "\nComandos Executados:"
        mapM_ (\(idNave, acao, params, estado) -> do
            putStrLn $ "ID: " ++ idNave ++ ", Ação: " ++ acao ++ ", Parâmetros: " ++ params ++ ", Posição: " ++ show estado) (reverse $ comandosExecutados reg)

-- Processar movimento da nave
processarMovimento :: String -> Nave -> Either String Nave
processarMovimento input nave = do
    let coords = words input
    case mapM readMaybe coords of
        Just [x, y, z] -> validarMovimento nave (x, y, z)
        _ -> Left "Coordenadas inválidas! Use o formato: x y z"

-- Executar ações para uma nave específica
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
            let registroInicial = Registro [] nave
            registroAtualizado <- menuAcoes nave registroInicial
            return $ map (\n -> if naveId n == id then estadoFinal registroAtualizado else n) naves

-- Executar ações para todas as naves
executarAcoesTodasNaves :: [Nave] -> IO [Nave]
executarAcoesTodasNaves naves = do
    putStrLn "\nExecutando ações para todas as naves..."
    foldM
        ( \navesAtuais nave -> do
            putStrLn $ "\nExecutando ações para nave " ++ naveId nave
            let registroInicial = Registro [] nave
            registroAtualizado <- menuAcoes nave registroInicial
            return $ map (\n -> if naveId n == naveId nave then estadoFinal registroAtualizado else n) navesAtuais
        )
        naves
        naves

-- Entrada direta de comandos
entradaDireta :: [Nave] -> IO [Nave]
entradaDireta naves = do
    putStrLn "\nDigite o comando no formato: ID COMANDO PARAMETROS"
    putStrLn "Exemplo: nave1 acao ligar"
    putStrLn "Exemplo: nave1 mover 10 20 30"
    comandosIntroduzidos <- loopEntrada naves []
    
    -- Exibir os comandos introduzidos
    putStrLn "\nInstruções introduzidas:"
    mapM_ (putStrLn . formatarComando) comandosIntroduzidos
    
    return naves
  where
    loopEntrada :: [Nave] -> [(String, String, String, Posicao)] -> IO [(String, String, String, Posicao)]
    loopEntrada naves comandos = do
        putStr "Introduza uma instrução para uma nave ou escreva 'voltar' para voltar ao menu: "
        hFlush stdout
        input <- getLine
        
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

    processarComando :: Nave -> String -> [String] -> [(String, String, String, Posicao)] -> IO [(String, String, String, Posicao)]
    processarComando nave cmd params comandos = case cmd of
        "mover" -> case params of
            [x, y, z] -> case (readMaybe x, readMaybe y, readMaybe z) of
                (Just dx, Just dy, Just dz) -> do
                    case validarMovimento nave (dx, dy, dz) of
                        Left erro -> do
                            putStrLn erro
                            loopEntrada naves comandos
                        Right naveAtualizada -> do
                            let novaPosicaoFinal = posicao naveAtualizada
                            let novoComando = (naveId nave, "mover", show (dx, dy, dz), novaPosicaoFinal)
                            loopEntrada naves (novoComando : comandos)
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
                        loopEntrada naves (novoComando : comandos)
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

-- Main program loop
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

-- Programa Principal
main :: IO ()
main = do
    putStrLn "A carregar arquivo de naves..."
    conteudo <- readFile "Alienship.txt"
    let naves = parseNave conteudo
    putStrLn "Arquivo carregado com sucesso!"
    loopPrincipal naves
    