{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Eta reduce" #-}
module Main where

import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import Text.Read (readMaybe)

type Posicao = (Int, Int, Int)
type Espaco = (Posicao, Posicao)

data Nave = Nave
    { naveId :: String
    , posicao :: Posicao
    , ligado :: Bool
    , espacoPermitido :: Espaco
    }
    deriving (Show, Eq)

data Registo = Registo
    { comandosExecutados :: [(String, String, String, Posicao)]
    , estadoFinal :: Nave
    }

mesmasPosicoes :: Posicao -> Posicao -> Bool
mesmasPosicoes (x1, y1, z1) (x2, y2, z2) = x1 == x2 && y1 == y2 && z1 == z2

verificarColisao :: [Nave] -> Nave -> Posicao -> Bool
verificarColisao todasNaves naveAtual novaPosicao =
    any
        ( \outraNave ->
            naveId outraNave /= naveId naveAtual
                && mesmasPosicoes novaPosicao (posicao outraNave)
        )
        todasNaves

lerCoordenadas :: String -> Maybe Posicao
lerCoordenadas str = do
    let cleanStr = filter (`notElem` "()") str
    let parts = words $ map (\c -> if c == ',' then ' ' else c) cleanStr
    case parts of
        [x, y, z] -> case (readMaybe x, readMaybe y, readMaybe z) of
            (Just x', Just y', Just z') -> Just (x', y', z')
            _ -> Nothing
        _ -> Nothing

formatarPosicao :: Posicao -> String
formatarPosicao (x, y, z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

parseNave :: String -> [Nave]
parseNave conteudo = foldl verificarColisoes [] (map parseLinha (lines conteudo))
  where
    defaultPosition = (0, 0, 0)

    -- Parse a single line into a Nave
    parseLinha linha = case words linha of
        (id : comandos) -> processarComandos (Nave id defaultPosition False ((0, 0, 0), (100, 100, 100))) comandos
        _ -> Nave "ERRO" defaultPosition False ((0, 0, 0), (100, 100, 100))

    -- Check for collisions and assign alternate positions
    verificarColisoes :: [Nave] -> Nave -> [Nave]
    verificarColisoes naves naveAtual
        | any (mesmasPosicoes (posicao naveAtual) . posicao) naves =
            -- If there's a collision, find the next available position
            let novaPos = encontrarProximaPosicaoLivre naves (posicao naveAtual)
             in naveAtual{posicao = novaPos} : naves
        | otherwise = naveAtual : naves

    -- Find the next available position by incrementing coordinates
    encontrarProximaPosicaoLivre :: [Nave] -> Posicao -> Posicao
    encontrarProximaPosicaoLivre naves pos@(x, y, z) =
        if any (mesmasPosicoes pos . posicao) naves
            then encontrarProximaPosicaoLivre naves (x + 1, y + 1, z + 1)
            else pos

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

verificarEspacoPermitido :: Posicao -> Nave -> Bool
verificarEspacoPermitido (x, y, z) nave =
    let ((minX, minY, minZ), (maxX, maxY, maxZ)) = espacoPermitido nave
     in x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ

ligarNave :: Nave -> Either String Nave
ligarNave nave =
    if ligado nave
        then Left "Erro: A nave já está ligada!"
        else Right nave{ligado = True}

desligarNave :: Nave -> Either String Nave
desligarNave nave =
    if not (ligado nave)
        then Left "Erro: A nave já está desligada!"
        else Right nave{ligado = False}

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

mostrarNave :: Nave -> IO ()
mostrarNave nave = do
    putStrLn $ "\nNave ID: " ++ naveId nave
    putStrLn $ "Posição: " ++ show (posicao nave)
    putStrLn $ "Estado: " ++ if ligado nave then "Ligada" else "Desligada"
    putStrLn $ "Espaço permitido: " ++ show (espacoPermitido nave)
    putStrLn "----------------------------------------"

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

menuAcoes :: [Nave] -> Nave -> Registo -> IO Registo
menuAcoes todasNaves nave registro = do
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
                    menuAcoes todasNaves nave registro
                Right naveAtualizada -> do
                    putStrLn "Movimento realizado com sucesso!"
                    menuAcoes todasNaves naveAtualizada (atualizarRegistro registro "Mover nave" input estadoAntes naveAtualizada)
        "4" -> do
            mostrarRegistro registro
            return registro
        _ -> do
            putStrLn "Opção inválida!"
            menuAcoes todasNaves nave registro
  where
    processarAcao acao result estadoAntes = case result of
        Left erro -> do
            putStrLn erro
            menuAcoes todasNaves nave registro
        Right naveAtualizada -> do
            putStrLn "Ação realizada com sucesso!"
            menuAcoes todasNaves naveAtualizada (atualizarRegistro registro acao "" estadoAntes naveAtualizada)

    atualizarRegistro reg acao params antes depois =
        reg{comandosExecutados = (naveId antes, acao, params, posicao depois) : comandosExecutados reg, estadoFinal = depois}

    mostrarRegistro reg = do
        putStrLn "\nComandos Executados:"
        mapM_
            ( \(idNave, acao, params, estado) -> do
                putStrLn $ "ID: " ++ idNave ++ ", Ação: " ++ acao ++ ", Parâmetros: " ++ params ++ ", Posição: " ++ show estado
            )
            (reverse $ comandosExecutados reg)

processarMovimento :: [Nave] -> String -> Nave -> Either String Nave
processarMovimento todasNaves input nave = do
    let coords = words input
    case mapM readMaybe coords of
        Just [x, y, z] -> validarMovimento todasNaves nave (x, y, z)
        _ -> Left "Coordenadas inválidas! Use o formato: x y z"

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

    atualizarNave :: [Nave] -> Nave -> [Nave]
    atualizarNave naves naveAtualizada =
        map (\n -> if naveId n == naveId naveAtualizada then naveAtualizada else n) naves

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

main :: IO ()
main = do
    putStrLn "A carregar arquivo de naves..."
    conteudo <- readFile "Alienship.txt"
    let naves = parseNave conteudo
    putStrLn "Arquivo carregado com sucesso!"
    loopPrincipal naves