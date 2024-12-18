{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import System.IO
import System.Exit (exitSuccess)
import Text.Read (readMaybe)

-- Representação de uma nave
data Nave = Nave
  { naveId :: String
  , localizacao :: (Int, Int, Int)
  , ligado :: Bool
  , espacoPermitido :: ((Int, Int, Int), (Int, Int, Int))
  } deriving (Show, Eq)

-- Estado inicial da nave
estadoInicial :: Nave
estadoInicial = Nave
  { naveId = "N/A"
  , localizacao = (0, 0, 0)
  , ligado = False
  , espacoPermitido = ((0, 0, 0), (100, 100, 100))
  }

-- Função para validar e converter string em Int
lerNumero :: String -> Maybe Int
lerNumero = readMaybe

-- Função para ligar a nave
ligarNave :: Nave -> IO Nave
ligarNave nave
  | ligado nave = do
      putStrLn $ "A nave " ++ naveId nave ++ " já está ligada."
      return nave
  | otherwise = do
      putStrLn $ "Ligando a nave " ++ naveId nave ++ "."
      return nave { ligado = True }

-- Função para desligar a nave
desligarNave :: Nave -> IO Nave
desligarNave nave
  | not (ligado nave) = do
      putStrLn $ "A nave " ++ naveId nave ++ " já está desligada."
      return nave
  | otherwise = do
      putStrLn $ "Desligando a nave " ++ naveId nave ++ "."
      return nave { ligado = False }

-- Função para validar movimento
validarMovimento :: Nave -> (Int, Int, Int) -> Bool
validarMovimento nave (x, y, z) =
  let ((x1, y1, z1), (x2, y2, z2)) = espacoPermitido nave
  in x >= x1 && x <= x2 && y >= y1 && y <= y2 && z >= z1 && z <= z2

-- Função para mover a nave
moverNave :: Nave -> (Int, Int, Int) -> IO Nave
moverNave nave (dx, dy, dz)
  | not (ligado nave) = do
      putStrLn $ "A nave " ++ naveId nave ++ " está desligada. Não pode se mover."
      return nave
  | otherwise =
      let (x, y, z) = localizacao nave
          novaPosicao = (x + dx, y + dy, z + dz)
      in if validarMovimento nave novaPosicao
         then do
           putStrLn $ "Movendo a nave " ++ naveId nave ++ " para " ++ show novaPosicao ++ "."
           return nave { localizacao = novaPosicao }
         else do
           putStrLn $ "Movimento inválido para a nave " ++ naveId nave ++ "."
           return nave

-- Função para atualizar o espaço permitido
atualizarEspacoPermitido :: Nave -> ((Int, Int, Int), (Int, Int, Int)) -> Nave
atualizarEspacoPermitido nave novosLimites = nave { espacoPermitido = novosLimites }

-- Função para processar uma instrução para uma nave
processarInstrucao :: [String] -> Nave -> IO Nave
processarInstrucao ("init":x:y:z:estado:_) nave =
  case mapM lerNumero [x, y, z, estado] of
    Just [x', y', z', e] -> return nave { localizacao = (x', y', z'), ligado = e == 1 }
    _ -> do
      putStrLn "Instrução 'init' mal formatada."
      return nave
processarInstrucao ("initspace":x1:y1:z1:x2:y2:z2:_) nave =
  case mapM lerNumero [x1, y1, z1, x2, y2, z2] of
    Just [x1', y1', z1', x2', y2', z2'] -> 
      return $ atualizarEspacoPermitido nave ((x1', y1', z1'), (x2', y2', z2'))
    _ -> do
      putStrLn "Instrução 'initspace' mal formatada."
      return nave
processarInstrucao ("ligar":_) nave = ligarNave nave
processarInstrucao ("desligar":_) nave = desligarNave nave
processarInstrucao ("move":dx:dy:dz:_) nave =
  case mapM lerNumero [dx, dy, dz] of
    Just [dx', dy', dz'] -> moverNave nave (dx', dy', dz')
    _ -> do
      putStrLn "Movimento mal formatado."
      return nave
processarInstrucao _ nave = do
  putStrLn "Instrução desconhecida ou mal formatada."
  return nave

-- Função para mostrar o estado da nave
mostrarNave :: Nave -> String
mostrarNave nave =
  "ID: " ++ naveId nave ++
  ", Localização: " ++ show (localizacao nave) ++
  ", Ligado: " ++ show (ligado nave) ++
  ", Espaço Permitido: " ++ show (espacoPermitido nave)

-- Função para atualizar o Map de naves
atualizarMap :: Map String Nave -> Nave -> Map String Nave
atualizarMap naves nave = Map.insert (naveId nave) nave naves

-- Função para processar linhas de um arquivo
processarLinhas :: [String] -> Map String Nave -> IO (Map String Nave)
processarLinhas [] naves = return naves
processarLinhas (linha:rest) naves = do
  let palavras = words linha
  case palavras of
    (idNave:instrucao) -> do
      let naveAtual = Map.findWithDefault estadoInicial idNave naves
      naveAtualizada <- processarInstrucao instrucao naveAtual
      let naveFinal = naveAtualizada { naveId = idNave }
      processarLinhas rest (atualizarMap naves naveFinal)
    _ -> processarLinhas rest naves

-- Função para carregar naves a partir de um arquivo
carregarNaves :: FilePath -> IO (Map String Nave)
carregarNaves arquivo = do
  conteudo <- readFile arquivo
  processarLinhas (lines conteudo) Map.empty

-- Função para salvar estado das naves
salvarNaves :: FilePath -> Map String Nave -> IO ()
salvarNaves arquivo naves = do
  let linhas = map mostrarNave (Map.elems naves)
  writeFile arquivo (unlines linhas)
  putStrLn $ "Estado das naves salvo em " ++ arquivo ++ "."

-- Função para listar todas as naves
listarNaves :: Map String Nave -> IO ()
listarNaves naves = mapM_ (putStrLn . mostrarNave) (Map.elems naves)

-- Função principal do menu
menu :: Map String Nave -> IO ()
menu naves = do
  putStrLn "\nMenu:"
  putStrLn "1. Listar todas as naves"
  putStrLn "2. Salvar estado das naves"
  putStrLn "3. Sair"
  putStr "Escolha uma opção: "
  opcao <- getLine
  case opcao of
    "1" -> do
      listarNaves naves
      menu naves
    "2" -> do
      salvarNaves "estado_atualizado.txt" naves
      menu naves
    "3" -> exitSuccess
    _ -> do
      putStrLn "Opção inválida."
      menu naves

-- Função principal
main :: IO ()
main = do
  putStrLn "Carregando naves a partir de 'naves.txt'..."
  naves <- carregarNaves "naves.txt"
  menu naves
