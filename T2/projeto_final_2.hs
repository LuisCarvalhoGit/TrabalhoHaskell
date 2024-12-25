module Main where

import System.IO (readFile, writeFile)
import Data.List (find, intercalate)
import Control.Monad (when)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

type Posicao = (Int, Int, Int)
type Espaco = (Posicao, Posicao)
type Movimento = Posicao

-- Representação de uma nave
data Nave = Nave {
    naveId :: String,
    posicao :: Posicao,
    ligado :: Bool,
    espacoPermitido :: Espaco
} deriving (Show, Eq)

-- Parse no ficheiro de input
parseNave :: String -> [Nave]
parseNave content = map parseLine (lines content)
  where
    parseLine line =
        let (idToken:rest) = words line
        in case rest of
            ("init" : x : y : z : p : _) ->
                Nave idToken (read x, read y, read z) (read p == 1) ((0, 0, 0), (100, 100, 100))
            ("initspace" : x1 : y1 : z1 : x2 : y2 : z2 : _) ->
                Nave idToken (0, 0, 0) False ((read x1, read y1, read z1), (read x2, read y2, read z2))
            _ -> error $ "Invalid line format: " ++ line

-- Opcoes do menu
data OpMenu = ListarTodas | ExecutarUma | ExecutarTodas | InputDireto | Sair deriving (Eq)

-- Mostrar menu principal
menuPrincipal :: IO OpMenu
menuPrincipal = do
    putStrLn "\n=== Menu de Controlo ==="
    putStrLn "\t1. Listar todas as naves"
    putStrLn "\t2. Executar acoes para uma nave"
    putStrLn "\t3. Executar acoes para todas as naves"
    putStrLn "\t4. Acao por input direto"
    putStrLn "\t5. Sair"
    putStr "Introduza a sua escolha: "
    escolha <- getLine
    case escolha of
        "1" -> return ListarTodas
        "2" -> return ExecutarUma
        "3" -> return ExecutarTodas
        "4" -> return InputDireto
        "5" -> return Sair
        _   -> putStrLn "Escolha Inválida!" >> menuPrincipal

-- Listar todas as naves
listarNaves :: [Nave] -> IO ()
listarNaves naves = do
    putStrLn "\n--- Lista das naves ---"
    mapM_ (putStrLn . naveId) naves

-- Executar acoes para uma nave
executarUma :: [Nave] -> IO [Nave]
executarUma naves = do
    putStr "Introduza o ID da nave: "
    nvId <- getLine
    let maybeNave = find ((== nvId) . naveId) naves
    case maybeNave of
        Nothing -> putStrLn "Nave não encontrada!" >> return naves
        Just nv -> do
            atualizado <- executarAcoes nv
            return $ map (\n -> if naveId n == nvId then atualizado else n) naves

-- Executar acoes para todas as naves
executarTodas :: [Nave] -> IO [Nave]
executarTodas = mapM executarAcoes

-- Acao por input direto
inputDireto :: [Nave] -> IO [Nave]
inputDireto naves = do
    putStr "Introduza o ID da nave: "
    nvId <- getLine
    let maybeNave = find ((== nvId) . naveId) naves
    case maybeNave of
        Nothing -> putStrLn "Nave não encontrada!" >> return naves
        Just nv -> do
            atualizado <- executarAcoes nv
            return $ map (\n -> if naveId n == nvId then atualizado else n) naves

-- Execute actions for a spacecraft
executarAcoes :: Nave -> IO Nave
executarAcoes nv = do
    putStrLn $ "\n--- A executar acoes para nave " ++ naveId nv ++ " ---"
    putStrLn "1. Ligar"
    putStrLn "2. Desligar"
    putStrLn "3. Mover"
    putStrLn "4. Sair"
    putStr "Introduzir acao: "
    acao <- getLine
    case acao of
        "1" ->
            if ligado nv
                then putStrLn "A nave ja esta ligada!" >> executarAcoes nv
                else return nv { ligado = True }
        "2" ->
            if not (ligado nv)
                then putStrLn "A nave ja esta desligada!" >> executarAcoes nv
                else return nv { ligado = False }
        "3" -> do
            putStrLn "Introduza o movimento (dx, dy, dz): "
            move <- getLine
            case parseMovimento move of
                Nothing -> putStrLn "Invalid movement format!" >> executarAcoes nv
                Just (dx, dy, dz) -> do
                    let novaPosicao = aplicarMovimento (posicao nv) (dx, dy, dz)
                        (minLim, maxLim) = espacoPermitido nv
                    if not (estaNoEspacoPermitido novaPosicao minLim maxLim)
                        then putStrLn "Movimento fora do espaco permitido!" >> executarAcoes nv
                        else return nv { posicao = novaPosicao }
        "4" -> return nv
        _   -> putStrLn "Acao invalida!" >> executarAcoes nv

-- Aplicar movimento
aplicarMovimento :: Posicao -> Posicao -> Posicao
aplicarMovimento (x, y, z) (dx, dy, dz) = (x + dx, y + dy, z + dz)

-- Verificar espaco permitido
estaNoEspacoPermitido :: Posicao -> Posicao -> Posicao -> Bool
estaNoEspacoPermitido (x, y, z) (minX, minY, minZ) (maxX, maxY, maxZ) =
    x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ

-- Parse movimento
parseMovimento :: String -> Maybe Posicao
parseMovimento input = case mapM readMaybe (words input) of
    Just [dx, dy, dz] -> Just (dx, dy, dz)
    _                 -> Nothing

-- Programa principal
main :: IO ()
main = do
    putStrLn "A carregar naves do ficheiro..."
    conteudo <- readFile "Alienship.txt"
    let naves = parseNave conteudo
    putStrLn "Naves carregadas com sucesso."
    mainLoop naves

mainLoop :: [Nave] -> IO ()
mainLoop naves = do
    opcao <- menuPrincipal
    case opcao of
        ListarTodas -> listarNaves naves >> mainLoop naves
        ExecutarUma -> do
            updated <- executarUma naves
            mainLoop updated
        ExecutarTodas -> do
            updated <- executarTodas naves
            mainLoop updated
        InputDireto -> do
            updated <- inputDireto naves
            mainLoop updated
        Sair -> putStrLn "A sair do programa. Adeus!"