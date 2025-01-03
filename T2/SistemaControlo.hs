module Main where

import Data.List
import System.IO
import Text.Read (readMaybe)

-- Tipos básicos
type Posicao = (Int, Int, Int)
type Espaco = (Posicao, Posicao)
type Movimento = Posicao

-- Estrutura da nave
data Nave = Nave
    { naveId :: String
    , posicao :: Posicao
    , ligado :: Bool
    , espacoPermitido :: Espaco
    }
    deriving (Show, Eq)

-- Função para ler o arquivo e criar naves
parseNave :: String -> [Nave]
parseNave conteudo = map parseLinha (lines conteudo)
  where
    parseLinha linha =
        let palavras = words linha
            naveId = head palavras -- Pega o ID da nave
            comandos = tail palavras -- Pega o resto da linha
         in parseComandos (Nave naveId (0, 0, 0) False ((0, 0, 0), (100, 100, 100))) comandos

    parseComandos :: Nave -> [String] -> Nave
    parseComandos nave [] = nave
    parseComandos nave (cmd : args) =
        case cmd of
            "init" -> case args of
                (pos : status : rest) ->
                    let posicao = lerCoordenadas pos
                        ligado = status == "1"
                     in parseComandos (nave{posicao = posicao, ligado = ligado}) rest
                _ -> nave
            "initspace" -> case args of
                (min : max : rest) ->
                    let espaco = (lerCoordenadas min, lerCoordenadas max)
                     in parseComandos (nave{espacoPermitido = espaco}) rest
                _ -> nave
            _ -> parseComandos nave (if null args then [] else tail args)

-- Função auxiliar para ler coordenadas do formato "(x,y,z)"
lerCoordenadas :: String -> Posicao
lerCoordenadas str =
    let numeros = filter (\c -> c `elem` "0123456789-,") str
        valores = map read $ split ',' numeros
     in case valores of
            [x, y, z] -> (x, y, z)
            _ -> (0, 0, 0)

-- Função auxiliar para dividir string por vírgulas
split :: Char -> String -> [String]
split c s = case break (== c) s of
    (x, _ : xs) -> x : split c xs
    (x, _) -> [x]

-- Menu principal
menuPrincipal :: IO String
menuPrincipal = do
    putStrLn "\n=== Menu de Controle ==="
    putStrLn "1. Listar todas as naves"
    putStrLn "2. Executar ações para uma nave"
    putStrLn "3. Executar ações para todas as naves"
    putStrLn "4. Entrada direta de comandos"
    putStrLn "5. Sair"
    putStr "Escolha uma opção: "
    getLine

-- Listar naves
mostrarNaves :: [Nave] -> IO ()
mostrarNaves naves = do
    putStrLn "\nLista de Naves:"
    mapM_ mostrarNave naves
  where
    mostrarNave nave = do
        putStrLn $ "ID: " ++ naveId nave
        putStrLn $ "Posição: " ++ show (posicao nave)
        putStrLn $ "Ligado: " ++ if ligado nave then "Sim" else "Não"
        putStrLn $ "Espaço permitido: " ++ show (espacoPermitido nave)
        putStrLn ""

mostrarNaveDetalhada :: Nave -> IO ()
mostrarNaveDetalhada nave = do
    putStrLn $ "\nNave ID: " ++ naveId nave
    putStrLn $ "Posição atual: " ++ show (posicao nave)
    putStrLn $ "Estado: " ++ if ligado nave then "Ligada" else "Desligada"
    putStrLn $ "Espaço permitido: " ++ show (espacoPermitido nave)
    putStrLn "----------------------------------------"

-- Funções para controlar uma nave
ligarNave :: Nave -> Nave
ligarNave nave = nave{ligado = True}

desligarNave :: Nave -> Nave
desligarNave nave = nave{ligado = False}

pedirMovimento :: IO Movimento
pedirMovimento = do
    putStrLn "Insira o movimento:"
    putStr "X: "
    x <- readLn
    putStr "Y: "
    y <- readLn
    putStr "Z: "
    z <- readLn
    return (x, y, z)

moverNave :: Nave -> Movimento -> Maybe Nave
moverNave nave (dx, dy, dz) =
    let (x, y, z) = posicao nave
        novaPosicao = (x + dx, y + dy, z + dz)
     in if not (ligado nave)
            then Nothing -- Nave desligada não pode mover
            else
                if verificarEspaco novaPosicao (espacoPermitido nave)
                    then Just nave{posicao = novaPosicao}
                    else Nothing

-- Verificar se posição está dentro do espaço permitido
verificarEspaco :: Posicao -> Espaco -> Bool
verificarEspaco (x, y, z) ((x1, y1, z1), (x2, y2, z2)) =
    x >= x1 && x <= x2 && y >= y1 && y <= y2 && z >= z1 && z <= z2

-- Menu de ações para uma nave
menuAcoes :: Nave -> IO Nave
menuAcoes nave = do
    putStrLn "\n=== Ações Disponíveis ==="
    putStrLn "1. Ligar nave"
    putStrLn "2. Desligar nave"
    putStrLn "3. Mover nave"
    putStrLn "4. Voltar"
    putStr "Escolha uma ação: "
    opcao <- getLine
    case opcao of
        "1" ->
            if ligado nave
                then do
                    putStrLn "Nave já está ligada!"
                    return nave
                else do
                    putStrLn "Nave ligada com sucesso!"
                    return $ nave{ligado = True}
        "2" ->
            if not (ligado nave)
                then do
                    putStrLn "Nave já está desligada!"
                    return nave
                else do
                    putStrLn "Nave desligada com sucesso!"
                    return $ nave{ligado = False}
        "3" -> do
            if not (ligado nave)
                then do
                    putStrLn "Erro: Nave precisa estar ligada para mover!"
                    return nave
                else do
                    movimento <- pedirMovimento
                    case moverNave nave movimento of
                        Just naveMoved -> do
                            putStrLn "Nave movida com sucesso!"
                            putStrLn $ "Nova posição: " ++ show (posicao naveMoved)
                            return naveMoved
                        Nothing -> do
                            putStrLn "Erro: Movimento fora do espaço permitido!"
                            return nave
        _ -> return nave

-- Programa principal
main :: IO ()
main = do
    putStrLn "Carregando arquivo de naves..."
    conteudo <- readFile "Alienship.txt"
    let naves = parseNave conteudo
    mapM_ mostrarNaveDetalhada naves
    loopPrincipal naves

loopPrincipal :: [Nave] -> IO ()
loopPrincipal naves = do
    opcao <- menuPrincipal
    case opcao of
        "1" -> do
            mostrarNaves naves
            loopPrincipal naves
        "2" -> do
            putStr "Digite o ID da nave: "
            id <- getLine
            case find (\n -> naveId n == id) naves of
                Just nave -> do
                    naveAtualizada <- menuAcoes nave
                    let navesAtualizadas = map (\n -> if naveId n == id then naveAtualizada else n) naves
                    loopPrincipal navesAtualizadas
                Nothing -> putStrLn "Nave não encontrada!" >> loopPrincipal naves
        "3" ->
            -- TODO: implementar ações para todas as naves
            loopPrincipal naves
        "4" ->
            -- TODO: implementar entrada direta de comandos
            loopPrincipal naves
        "5" -> putStrLn "Programa finalizado!"
        _ -> putStrLn "Opção inválida!" >> loopPrincipal naves