{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Main where

import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import Text.Read (readMaybe)

-- Basic types for representing ship positions and allowed space
type Posicao = (Int, Int, Int)
type Espaco = (Posicao, Posicao)

-- Main data structure for representing a ship
data Nave = Nave
    { naveId :: String -- Unique identifier for each ship
    , posicao :: Posicao -- Current position (x, y, z)
    , ligado :: Bool -- Power status (on/off)
    , espacoPermitido :: Espaco -- Allowed movement space ((minX,minY,minZ), (maxX,maxY,maxZ))
    }
    deriving (Show, Eq)

-- Parse coordinates from string format "(x,y,z)"
lerCoordenadas :: String -> Maybe Posicao
lerCoordenadas str = do
    let cleanStr = filter (`notElem` "()") str
    let parts = words $ map (\c -> if c == ',' then ' ' else c) cleanStr
    case parts of
        [x, y, z] -> case (readMaybe x, readMaybe y, readMaybe z) of
            (Just x', Just y', Just z') -> Just (x', y', z')
            _ -> Nothing
        _ -> Nothing

-- Parse ship information from file content
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
                                (0, 0, 0)
                                args
                     in case validarMovimento nave summedCoords of
                            Right naveAtualizada -> naveAtualizada
                            Left _ -> nave -- Retain the old state if invalid
                "acao" -> case args of
                    (acao : rest) ->
                        let acaoLimpa = filter (`notElem` "()") acao
                         in case acaoLimpa of
                                "ligar" -> nave{ligado = True}
                                "desligar" -> nave{ligado = False}
                                _ -> nave
                    _ -> nave -- No valid "acao", return current state
                _ -> nave -- For unrecognized commands, return current state
         in processarComandos updatedNave args

-- Validation functions
validarMovimento :: Nave -> Posicao -> Either String Nave
validarMovimento nave (dx, dy, dz) =
    if not (ligado nave)
        then Left "Erro: A nave precisa estar ligada para mover!"
        else
            let (x, y, z) = posicao nave
                novaPosicao = (x + dx, y + dy, z + dz)
             in if verificarEspacoPermitido novaPosicao nave
                    then Right nave{posicao = novaPosicao}
                    else Left "Erro: Movimento fora do espaço permitido!"

verificarEspacoPermitido :: Posicao -> Nave -> Bool
verificarEspacoPermitido (x, y, z) nave =
    let ((minX, minY, minZ), (maxX, maxY, maxZ)) = espacoPermitido nave
     in x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ

-- Ship control functions
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

-- Save ships state to file
salvarNaves :: FilePath -> [Nave] -> IO ()
salvarNaves caminho naves = do
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
        formatarPosicao (x, y, z) = show x ++ "," ++ show y ++ "," ++ show z

-- Display ship information
mostrarNave :: Nave -> IO ()
mostrarNave nave = do
    putStrLn $ "\nNave ID: " ++ naveId nave
    putStrLn $ "Posição: " ++ show (posicao nave)
    putStrLn $ "Estado: " ++ if ligado nave then "Ligada" else "Desligada"
    putStrLn $ "Espaço permitido: " ++ show (espacoPermitido nave)
    putStrLn "----------------------------------------"

-- Improved menu functions with better error handling
menuPrincipal :: IO (Maybe Int)
menuPrincipal = do
    putStrLn "\n=== Sistema de Controle de Nave Alienígena ==="
    putStrLn "1. Listar todas as naves"
    putStrLn "2. Executar ações para uma nave"
    putStrLn "3. Executar ações para todas as naves"
    putStrLn "4. Entrada direta de comandos"
    putStrLn "5. Sair"
    putStr "Escolha uma opção: "
    hFlush stdout
    input <- getLine
    return $ readMaybe input

-- Improved actions menu with better feedback
menuAcoes :: Nave -> IO Nave
menuAcoes nave = do
    putStrLn "\n=== Menu de Ações ==="
    putStrLn "1. Ligar nave"
    putStrLn "2. Desligar nave"
    putStrLn "3. Mover nave"
    putStrLn "4. Voltar"
    putStr "Escolha uma ação: "
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> processarAcao $ ligarNave nave
        "2" -> processarAcao $ desligarNave nave
        "3" -> do
            putStrLn "Digite o movimento (x y z):"
            input <- getLine
            case processarMovimento input nave of
                Left erro -> do
                    putStrLn erro
                    menuAcoes nave
                Right naveAtualizada -> return naveAtualizada
        "4" -> return nave
        _ -> do
            putStrLn "Opção inválida!"
            menuAcoes nave
  where
    processarAcao result = case result of
        Left erro -> do
            putStrLn erro
            menuAcoes nave
        Right naveAtualizada -> do
            putStrLn "Ação realizada com sucesso!"
            return naveAtualizada

-- Helper function to process movement input
processarMovimento :: String -> Nave -> Either String Nave
processarMovimento input nave = do
    let coords = words input
    case mapM readMaybe coords of
        Just [x, y, z] -> validarMovimento nave (x, y, z)
        _ -> Left "Coordenadas inválidas! Use o formato: x y z"

-- Execute actions for a single ship
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
            naveAtualizada <- menuAcoes nave
            return $ map (\n -> if naveId n == id then naveAtualizada else n) naves

-- Execute actions for all ships
executarAcoesTodasNaves :: [Nave] -> IO [Nave]
executarAcoesTodasNaves naves = do
    putStrLn "\nExecutando ações para todas as naves..."
    foldM
        ( \navesAtuais nave -> do
            putStrLn $ "\nExecutando ações para nave " ++ naveId nave
            naveAtualizada <- menuAcoes nave
            return $ map (\n -> if naveId n == naveId nave then naveAtualizada else n) navesAtuais
        )
        naves
        naves

-- Direct command input handling
entradaDireta :: [Nave] -> IO [Nave]
entradaDireta naves = do
    putStrLn "\nDigite o comando no formato: ID COMANDO PARAMETROS"
    putStrLn "Exemplo: nave1 ligar"
    putStrLn "         nave1 mover 10 20 30"
    input <- getLine
    let palavras = words input
    case palavras of
        (id : cmd : params) ->
            case find (\n -> naveId n == id) naves of
                Nothing -> do
                    putStrLn "Nave não encontrada!"
                    return naves
                Just nave ->
                    processarComandoDireto nave cmd params naves
        _ -> do
            putStrLn "Formato de comando inválido!"
            return naves

-- Process direct commands
processarComandoDireto :: Nave -> String -> [String] -> [Nave] -> IO [Nave]
processarComandoDireto nave cmd params naves = case cmd of
    "ligar" -> processarResultado $ ligarNave nave
    "desligar" -> processarResultado $ desligarNave nave
    "mover" -> case params of
        [x, y, z] -> case (readMaybe x, readMaybe y, readMaybe z) of
            (Just dx, Just dy, Just dz) ->
                processarResultado $ validarMovimento nave (dx, dy, dz)
            _ -> comandoInvalido "Parâmetros de movimento inválidos!"
        _ -> comandoInvalido "Número incorreto de parâmetros para movimento!"
    _ -> comandoInvalido "Comando desconhecido!"
  where
    processarResultado result = case result of
        Left erro -> do
            putStrLn erro
            return naves
        Right naveAtualizada -> do
            putStrLn "Comando executado com sucesso!"
            return $ map (\n -> if naveId n == naveId nave then naveAtualizada else n) naves

    comandoInvalido msg = do
        putStrLn msg
        return naves

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
                salvarNaves "AlienshipSave.txt" naves
                putStrLn "Estado salvo com sucesso!"
                putStrLn "Encerrando o programa..."
            _ -> do
                putStrLn "Opção inválida! Escolha entre 1 e 5."
                loopPrincipal naves

-- Main program
main :: IO ()
main = do
    putStrLn "Carregando arquivo de naves..."
    conteudo <- readFile "Alienship.txt"
    let naves = parseNave conteudo
    putStrLn "Arquivo carregado com sucesso!"
    loopPrincipal naves
