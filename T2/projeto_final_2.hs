module Main where

import System.IO (readFile)
import Data.List (find)
import Text.Read (readMaybe)
import Control.Monad (foldM)
import Data.Maybe (catMaybes)

type Posicao = (Int, Int, Int)
type Espaco = (Posicao, Posicao)

-- Representação de uma nave
data Nave = Nave {
    naveId :: String,
    posicao :: Posicao,
    ligado :: Bool,
    espacoPermitido :: Espaco
} deriving (Show, Eq)

-- Parse no ficheiro de input
parseNave :: String -> [Nave]
parseNave content = concatMap parseLine (lines content)
  where
    parseLine line =
        let (idToken:rest) = words line
        in case rest of
            ("init" : x : y : z : p : _) -> 
                case (readMaybe x, readMaybe y, readMaybe z, readMaybe p) of
                    (Just x', Just y', Just z', Just p') ->
                        [Nave idToken (x', y', z') (p' == 1) ((0, 0, 0), (100, 100, 100))]
                    _ -> 
                        printInvalidLine line >> return [Nave idToken (0, 0, 0) False ((0, 0, 0), (100, 100, 100))]  -- Retorna uma nave padrão
            ("initspace" : x1 : y1 : z1 : x2 : y2 : z2 : _) ->
                case (readMaybe x1, readMaybe y1, readMaybe z1, readMaybe x2, readMaybe y2, readMaybe z2) of
                    (Just x1', Just y1', Just z1', Just x2', Just y2', Just z2') ->
                        [Nave idToken (0, 0, 0) False ((x1', y1', z1'), (x2', y2', z2'))]
                    _ -> 
                        printInvalidLine line >> return [Nave idToken (0, 0, 0) False ((0, 0, 0), (100, 100, 100))]  -- Retorna uma nave padrão
            _ -> 
                printInvalidLine line >> return []  -- Retorna uma lista vazia para ignorar a linha inválida

-- Função auxiliar para imprimir linhas inválidas
printInvalidLine :: String -> IO ()
printInvalidLine line = putStrLn $ "Formato de linha inválido: " ++ line


-- Opções do menu
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
    mapM_ imprimirEstadoNave naves  -- Imprime o estado de cada nave
    putStrLn "Pressione Enter para voltar ao menu."
    _ <- getLine  -- Espera pela entrada do usuário
    return ()  -- Volta ao menu


-- Imprimir o estado da nave
imprimirEstadoNave :: Nave -> IO ()
imprimirEstadoNave nave = do
    putStrLn $ " Estado Atual da Nave: "
    putStrLn $ "ID: " ++ naveId nave
    putStrLn $ "Posição: " ++ show (posicao nave)
    putStrLn $ "Ligada: " ++ show (ligado nave)
    putStrLn $ "Espaço Permitido: " ++ show (espacoPermitido nave)
    putStrLn ""

-- Executar acoes para uma nave
executarUma :: [Nave] -> IO [Nave]
executarUma naves = do
    putStr "Introduza o ID da nave: "
    nvId <- getLine
    let maybeNave = find ((== nvId) . naveId) naves
    case maybeNave of
        Nothing -> putStrLn "Nave não encontrada!" >> return naves
        Just nv -> do
            atualizado <- executarAcoes [nv] naves  -- Passa a lista com a nave encontrada
            let naveAtualizada = head atualizado
            imprimirEstadoNave naveAtualizada  -- Imprime o estado atualizado da nave
            return $ map (\n -> if naveId n == nvId then naveAtualizada else n) naves 

-- Instruções de entrada manual
instrucoesEntradaManual :: [Nave] -> IO [Nave]
instrucoesEntradaManual naves = do
    putStrLn "Introduza as instruções para a nave (formato: ID comando args):"
    input <- getLine
    let partes = words input
    case partes of
        (idNave:comando:args) -> do
            let maybeNave = find ((== idNave) . naveId) naves
            case maybeNave of
                Nothing -> putStrLn "Nave não encontrada!" >> instrucoesEntradaManual naves
                Just nv -> case comando of
                    "ligar" -> do
                        let atualizado = map (\n -> if naveId n == idNave then nv { ligado = True } else n) naves
                        imprimirEstadoNave (head atualizado)  -- Imprime o estado atualizado
                        return atualizado
                    "desligar" -> do
                        let atualizado = map (\n -> if naveId n == idNave then nv { ligado = False } else n) naves
                        imprimirEstadoNave (head atualizado)  -- Imprime o estado atualizado
                        return atualizado
                    "init" -> case map readMaybe args of
                        [Just x, Just y, Just z, Just p] -> do
                            let novaPosicao = (x, y, z)
                            let atualizado = map (\n -> if naveId n == idNave then nv { posicao = novaPosicao, ligado = p == 1 } else n) naves
                            imprimirEstadoNave (head atualizado)  -- Imprime o estado atualizado
                            return atualizado
                        _ -> putStrLn "Formato de inicialização inválido!" >> instrucoesEntradaManual naves
                    "initspace" -> case map readMaybe args of
                        [Just x1, Just y1, Just z1, Just x2, Just y2, Just z2] -> do
                            let novoEspaco = ((x1, y1, z1), (x2, y2, z2))
                            let atualizado = map (\n -> if naveId n == idNave then nv { espacoPermitido = novoEspaco } else n) naves
                            imprimirEstadoNave (head atualizado)  -- Imprime o estado atualizado
                            return atualizado
                        _ -> putStrLn "Formato de espaço inválido!" >> instrucoesEntradaManual naves
                    "move" -> case map readMaybe args of
                        [Just dx, Just dy, Just dz] -> do
                            let (x, y, z) = posicao nv
                            let novaPosicao = (x + dx, y + dy, z + dz)  -- Adiciona as novas coordenadas
                            if estaNoEspacoPermitido novaPosicao (espacoPermitido nv)
                                then do
                                    let atualizado = map (\n -> if naveId n == idNave then nv { posicao = novaPosicao } else n) naves
                                    imprimirEstadoNave (head atualizado)  -- Imprime o estado atualizado
                                    return atualizado
                                else putStrLn "Nova posição fora do espaço permitido!" >> instrucoesEntradaManual naves
                        _ -> putStrLn "Formato de movimento inválido! Use: move dx dy dz" >> instrucoesEntradaManual naves
                    _ -> putStrLn "Comando inválido!" >> instrucoesEntradaManual naves
        _ -> putStrLn "Formato inválido!" >> instrucoesEntradaManual naves

-- Executar acoes para todas as naves
executarTodas :: [Nave] -> IO [Nave]
executarTodas naves = foldM (\acc nv -> executarAcoes [nv] acc) naves naves

-- Acao por input direto
inputDireto :: [Nave] -> IO [Nave]
inputDireto naves = do
    putStr "Introduza o ID da nave: "
    nvId <- getLine
    let maybeNave = find ((== nvId) . naveId) naves
    case maybeNave of
        Nothing -> putStrLn "Nave não encontrada!" >> return naves
        Just nv -> do
            atualizado <- executarAcoes [nv] naves
            let naveAtualizada = head atualizado
            imprimirEstadoNave naveAtualizada  -- Imprime o estado atualizado da nave
            return $ map (\n -> if naveId n == nvId then naveAtualizada else n) naves

-- Executar ações para uma nave
executarAcoes :: [Nave] -> [Nave] -> IO [Nave]
executarAcoes [nv] naves = do
    putStrLn $ "\n--- A executar acoes para nave " ++ naveId nv ++ " ---"
    putStrLn "1. Ligar"
    putStrLn "2. Desligar"
    putStrLn "3. Instruções de entrada manual"
    putStrLn "4. Sair"
    putStr "Introduzir acao: "
    acao <- getLine
    case acao of
        "1" ->
            if ligado nv
                then putStrLn "A nave já está ligada!" >> executarAcoes [nv] naves
                else do
                    let atualizado = map (\n -> if naveId n == naveId nv then nv { ligado = True } else n) naves
                    imprimirEstadoNave (head atualizado)  -- Imprime o estado atualizado
                    return atualizado
        "2" ->
            if not (ligado nv)
                then putStrLn "A nave já está desligada!" >> executarAcoes [nv] naves
                else do
                    let atualizado = map (\n -> if naveId n == naveId nv then nv { ligado = False } else n) naves
                    imprimirEstadoNave (head atualizado)  -- Imprime o estado atualizado
                    return atualizado
        "3" -> instrucoesEntradaManual naves  -- Passa a lista completa de naves
        "4" -> return naves
        _   -> putStrLn "Ação inválida!" >> executarAcoes [nv] naves

-- Verificar espaco permitido
estaNoEspacoPermitido :: Posicao -> Espaco -> Bool
estaNoEspacoPermitido (x, y, z) ((minX, minY, minZ), (maxX, maxY , maxZ)) =
    x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ

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
