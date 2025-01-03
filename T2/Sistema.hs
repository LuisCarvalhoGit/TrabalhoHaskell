module Main where

import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import Text.Read (readMaybe)

-- Tipos básicos
type Posicao = (Int, Int, Int)
type Espaco = (Posicao, Posicao)

-- Estrutura da nave
data Nave = Nave
    { naveId :: String
    , posicao :: Posicao
    , ligado :: Bool
    , espacoPermitido :: Espaco
    }
    deriving (Show, Eq)

lerCoordenadas :: String -> Maybe Posicao
lerCoordenadas str = do
    let cleanStr = filter (`notElem` "()") str
    let parts = words $ map (\c -> if c == ',' then ' ' else c) cleanStr
    case parts of
        [x, y, z] -> case (readMaybe x, readMaybe y, readMaybe z) of
            (Just x', Just y', Just z') -> Just (x', y', z')
            _ -> Nothing
        _ -> Nothing

parseNave :: String -> [Nave]
parseNave conteudo = map parseLinha (lines conteudo)
  where
    parseLinha linha = case words linha of
        (id : comandos) -> do
            let naveInicial = Nave id (0, 0, 0) False ((0, 0, 0), (100, 100, 100))
            processarComandos naveInicial comandos
        _ -> Nave "ERRO" (0, 0, 0) False ((0, 0, 0), (100, 100, 100))

    processarComandos :: Nave -> [String] -> Nave
    processarComandos nave [] = nave
    processarComandos nave (cmd : args) =
        case cmd of
            "init" -> case args of
                (pos : status : rest) -> do
                    case lerCoordenadas pos of
                        Just posicaoInicial ->
                            let naveAtualizada = nave{posicao = posicaoInicial, ligado = status == "1"}
                             in processarComandos naveAtualizada rest
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
                let (movimentos, restoArgs) = coletarMovimentos args
                 in if null movimentos
                        then processarComandos nave args
                        else
                            let somaPosicoes = somarMovimentos movimentos
                                (x, y, z) = posicao nave
                                (dx, dy, dz) = somaPosicoes
                                novaPosicao = (x + dx, y + dy, z + dz)
                                naveAtualizada = nave{posicao = novaPosicao}
                             in processarComandos naveAtualizada restoArgs
            "acao" -> case args of
                (acao : rest) ->
                    let acaoLimpa = filter (`notElem` "()") acao
                     in case acaoLimpa of
                            "ligar" -> processarComandos (nave{ligado = True}) rest
                            "desligar" -> processarComandos (nave{ligado = False}) rest
                            _ -> processarComandos nave rest
                _ -> processarComandos nave args
            _ -> processarComandos nave args

coletarMovimentos :: [String] -> ([Posicao], [String])
coletarMovimentos [] = ([], [])
coletarMovimentos args =
    let (coords, rest) = span (\s -> "(" `isPrefixOf` s) args
        posicoes = mapMaybe lerCoordenadas coords
     in (posicoes, rest)

somarMovimentos :: [Posicao] -> Posicao
somarMovimentos movimentos =
    let (xs, ys, zs ) = unzip3 movimentos
     in (sum xs, sum ys, sum zs)

-- Funções de validação
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
     in x >= minX
            && x <= maxX
            && y >= minY
            && y <= maxY
            && z >= minZ
            && z <= maxZ

-- Funções de controle da nave
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

-- Função para salvar naves
salvarNaves :: FilePath -> [Nave] -> IO ()
salvarNaves caminho naves = do
    let conteudo = unlines $ map formatarNave naves
    writeFile caminho conteudo
  where
    formatarNave :: Nave -> String
    formatarNave nave =
        naveId nave ++ " " ++
        "init (" ++ show (x, y, z) ++ ") " ++
        (if ligado nave then "1" else "0") ++ " " ++
        "initspace (" ++ show (minX, minY, minZ) ++ ") (" ++ show (maxX, maxY, maxZ) ++ ")"
      where
        (x, y, z) = posicao nave
        ((minX, minY, minZ), (maxX, maxY, maxZ)) = espacoPermitido nave

-- Interface com usuário
mostrarNave :: Nave -> IO ()
mostrarNave nave = do
    putStrLn $ "\nNave ID: " ++ naveId nave
    putStrLn $ "Posição: " ++ show (posicao nave)
    putStrLn $ "Estado: " ++ if ligado nave then "Ligada" else "Desligada"
    putStrLn $ "Espaço permitido: " ++ show (espacoPermitido nave)
    putStrLn "----------------------------------------"

menuPrincipal :: IO Int
menuPrincipal = do
    putStrLn "\n=== Sistema de Controle de Nave Alienígena ==="
    putStrLn "1. Listar todas as naves"
    putStrLn "2. Executar ações para uma nave"
    putStrLn "3. Executar ações para todas as naves"
    putStrLn "4. Entrada direta de comandos"
    putStrLn "5. Sair"
    putStr "Escolha uma opção: "
    readLn

menuAcoes :: FilePath -> [Nave] -> Nave -> IO Nave
menuAcoes caminho naves nave = do
    putStrLn "\n=== Menu de Ações ==="
    putStrLn "1. Ligar nave"
    putStrLn "2. Desligar nave"
    putStrLn "3. Mover nave"
    putStrLn "4. Voltar"
    putStr "Escolha uma ação: "
    opcao <- getLine
    case opcao of
        "1" -> case ligarNave nave of
            Left erro -> do
                putStrLn erro
                return nave
            Right naveAtualizada -> do
                putStrLn "Nave ligada com sucesso!"
                salvarNaves caminho (naveAtualizada : filter ((/= naveId naveAtualizada) . naveId) naves)
                return naveAtualizada
        "2" -> case desligarNave nave of
            Left erro -> do
                putStrLn erro
                return nave
            Right naveAtualizada -> do
                putStrLn "Nave desligada com sucesso!"
                salvarNaves caminho (naveAtualizada : filter ((/= naveId naveAtualizada) . naveId) naves)
                return naveAtualizada
        "3" -> do
            putStrLn "Digite o movimento (x y z):"
            input <- getLine
            let coords = words input
            case map readMaybe coords of
                [Just x, Just y, Just z] ->
                    case validarMovimento nave (x, y, z) of
                        Left erro -> putStrLn erro >> return nave
                        Right naveAtualizada -> do
                            putStrLn "Movimento realizado com sucesso!"
                            salvarNaves caminho (naveAtualizada : filter ((/= naveId naveAtualizada) . naveId) naves)
                            return naveAtualizada
                _ -> putStrLn "Coordenadas inválidas!" >> return nave
        "4" -> return nave
        _ -> putStrLn "Opção inválida!" >> return nave

-- Funções principais do programa
executarAcoesUmaNave :: FilePath -> [Nave] -> IO [Nave]
executarAcoesUmaNave caminho naves = do
    putStr "Escreva o ID da nave: "
    id <- getLine
    case find (\n -> naveId n == id) naves of
        Nothing -> do
            putStrLn "Nave não encontrada!"
            return naves
        Just nave -> do
            naveAtualizada <- menuAcoes caminho naves nave
            return $ map (\n -> if naveId n == id then naveAtualizada else n) naves

executarAcoesTodasNaves :: FilePath -> [Nave] -> IO [Nave]
executarAcoesTodasNaves caminho naves = do
    putStrLn "\nA executar ações para todas as naves..."
    foldM
        ( \navesAtuais nave -> do
            putStrLn $ "\nExecutando ações para nave " ++ naveId nave
            naveAtualizada <- menuAcoes caminho naves nave
            return $ map (\n -> if naveId n == naveId nave then naveAtualizada else n) navesAtuais
        )
        naves
        naves

entradaDireta :: FilePath -> [Nave] -> IO [Nave]
entradaDireta caminho naves = do
    putStrLn "\nDigite o comando no formato: ID COMANDO PARAMETROS"
    putStrLn "Exemplo: nave1 ligar"
    putStrLn "        nave1 mover 10 20 30"
    input <- getLine
    let palavras = words input
    case palavras of
        (id : cmd : params) -> case find (\n -> naveId n == id) naves of
            Nothing -> putStrLn "Nave não encontrada!" >> return naves
            Just nave -> processarComandoDireto caminho nave cmd params naves
        _ -> putStrLn "Formato de comando inválido!" >> return naves

processarComandoDireto :: FilePath -> Nave -> String -> [String] -> [Nave] -> IO [Nave]
processarComandoDireto caminho nave cmd params naves = case cmd of
    "ligar" -> case ligarNave nave of
        Left erro -> putStrLn erro >> return naves
        Right naveAtualizada -> do
            putStrLn "Nave ligada com sucesso!"
            salvarNaves caminho (naveAtualizada : filter ((/= naveId naveAtualizada) . naveId) naves)
            return $ map (\n -> if naveId n == naveId nave then naveAtualizada else n) naves
    "desligar" -> case desligarNave nave of
        Left erro -> putStrLn erro >> return naves
        Right naveAtualizada -> do
            putStrLn "Nave desligada com sucesso!"
            salvarNaves caminho (naveAtualizada : filter ((/= naveId naveAtualizada) . naveId) naves)
            return $ map (\n -> if naveId n == naveId nave then naveAtualizada else n) naves
    "mover" -> case params of
        [x, y, z] -> case (readMaybe x, readMaybe y, readMaybe z) of
            (Just dx, Just dy, Just dz) ->
                case validarMovimento nave (dx, dy, dz) of
                    Left erro -> putStrLn erro >> return naves
                    Right naveAtualizada -> do
                        putStrLn "Movimento realizado com sucesso!"
                        salvarNaves caminho (naveAtualizada : filter ((/= naveId naveAtualizada) . naveId) naves)
                        return $ map (\n -> if naveId n == naveId nave then naveAtualizada else n) naves
            _ -> putStrLn "Parâmetros de movimento inválidos!" >> return naves
        _ -> putStrLn "Número incorreto de parâmetros para movimento!" >> return naves
    "init" -> case params of
        (posStr : status : rest) -> case lerCoordenadas posStr of
            Just pos -> do 
                let naveAtualizada = nave{posicao = pos, ligado = status == "1"}
                putStrLn "Inicialização da nave realizada com sucesso!"
                salvarNaves caminho (naveAtualizada : filter ((/= naveId naveAtualizada) . naveId) naves)
                return $ map (\n -> if naveId n == naveId nave then naveAtualizada else n) naves
            Nothing -> putStrLn "Coordenadas inválidas para inicialização!" >> return naves
        _ -> putStrLn "Parâmetros insuficientes para inicialização!" >> return naves
    "initspace" -> case params of
        (minStr : maxStr : rest) -> case (lerCoordenadas minStr, lerCoordenadas maxStr) of
            (Just minPos, Just maxPos) -> do
                let naveAtualizada = nave{espacoPermitido = (minPos, maxPos)}
                putStrLn "Espaço permitido inicializado com sucesso!"
                salvarNaves caminho (naveAtualizada : filter ((/= naveId naveAtualizada) . naveId) naves)
                return $ map (\n -> if naveId n == naveId nave then naveAtualizada else n) naves
            _ -> putStrLn "Coordenadas inválidas para espaço permitido!" >> return naves
        _ -> putStrLn "Parâmetros insuficientes para espaço permitido!" >> return naves
    _ -> do
        putStrLn "Comando desconhecido!"
        return naves

-- Loop principal do programa
loopPrincipal :: FilePath -> [Nave] -> IO ()
loopPrincipal caminho naves = do
    opcao <- menuPrincipal
    case opcao of
        1 -> do
            putStrLn "\nListando todas as naves:"
            mapM_ mostrarNave naves
            loopPrincipal caminho naves
        2 -> do
            navesAtualizadas <- executarAcoesUmaNave caminho naves
            loopPrincipal caminho navesAtualizadas
        3 -> do
            navesAtualizadas <- executarAcoesTodasNaves caminho naves
            loopPrincipal caminho navesAtualizadas
        4 -> do
            navesAtualizadas <- entradaDireta caminho naves
            loopPrincipal caminho navesAtualizadas
        5 -> do
            putStrLn "\nEncerrando o programa..."
        _ -> do
            putStrLn "Opção inválida!"
            loopPrincipal caminho naves

-- Programa principal
main :: IO ()
main = do
    let caminho = "Alienship.txt"
    putStrLn "Carregando arquivo de naves..."
    conteudo <- readFile caminho
    let naves = parseNave conteudo
    putStrLn "Arquivo carregado com sucesso!"
    loopPrincipal caminho naves