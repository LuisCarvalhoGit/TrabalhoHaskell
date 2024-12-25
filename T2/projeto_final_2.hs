{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Control.Monad (when, foldM)
import System.IO (withFile, IOMode(..), hPutStrLn, hGetContents)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)

-- Representação de uma nave
data Nave = Nave
  { naveId :: String
  , localizacao :: (Int, Int, Int)
  , ligado :: Bool
  , espacoPermitido :: ((Int, Int, Int), (Int, Int, Int))
  } deriving (Show, Eq)

-- Função para validar e converter string em Int
lerNumero :: String -> Maybe Int
lerNumero = readMaybe

-- Função para validar movimento
validarMovimento :: Nave -> (Int, Int, Int) -> Bool
validarMovimento nave (dx, dy, dz) =
  let (x, y, z) = localizacao nave
      novaPosicao = (x + dx, y + dy, z + dz)
      ((x1, y1, z1), (x2, y2, z2)) = espacoPermitido nave
  in novaPosicao `within` ((x1, y1, z1), (x2, y2, z2)) && (z + dz) >= 0

within :: (Int, Int, Int) -> ((Int, Int, Int), (Int, Int, Int)) -> Bool
within (x, y, z) ((x1, y1, z1), (x2, y2, z2)) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2 && z >= z1 && z <= z2

-- Função para processar uma instrução para uma nave
processarInstrucao :: [String] -> Map String Nave -> IO (Map String Nave)
processarInstrucao ("init":idNave:x:y:z:estado:_) naves =
  case mapM lerNumero [x, y, z, estado] of
    Just [x', y', z', e] -> 
      let novaNave = Nave idNave (x', y', z') (e == 1) ((0, 0, 0), (100, 100, 100))
      in return $ Map.insert idNave novaNave naves
    _ -> do
      putStrLn "Instrução 'init' mal formatada."
      return naves
processarInstrucao ("initspace":idNave:x1:y1:z1:x2:y2:z2:_) naves =
  case mapM lerNumero [x1, y1, z1, x2, y2, z2] of
    Just [x1', y1', z1', x2', y2', z2'] -> 
      case Map.lookup idNave naves of
        Just nave -> 
          let novaNave = nave { espacoPermitido = ((x1', y1', z1'), (x2', y2', z2')) }
          in return $ Map.insert idNave novaNave naves
        Nothing -> do
          putStrLn "Nave não encontrada."
          return naves
    _ -> do
      putStrLn "Instrução 'initspace' mal formatada."
      return naves
processarInstrucao ("ligar":idNave:_) naves =
  case Map.lookup idNave naves of
    Just nave | not (ligado nave) -> do
      putStrLn $ "Ligando a nave " ++ naveId nave ++ "."
      let novaNave = nave { ligado = True }
      return $ Map.insert idNave novaNave naves
    Just nave -> do
      putStrLn $ "A nave " ++ naveId nave ++ " já está ligada."
      return naves
    Nothing -> do
      putStrLn "Nave não encontrada."
      return naves
processarInstrucao ("desligar":idNave:_) naves =
  case Map.lookup idNave naves of
    Just nave | ligado nave -> do
      putStrLn $ "Desligando a nave " ++ naveId nave ++ "."
      let novaNave = nave { ligado = False }
      return $ Map.insert idNave novaNave naves
    Just nave -> do
      putStrLn $ "A nave " ++ naveId nave ++ " já está desligada."
      return naves
    Nothing -> do
      putStrLn "Nave não encontrada."
      return naves
processarInstrucao ("mover":idNave:dx:dy:dz:_) naves =
  case Map.lookup idNave naves of
    Just nave -> 
      case mapM lerNumero [dx, dy, dz] of
        Just [dx', dy', dz'] -> 
          let (x, y, z) = localizacao nave
              novaPosicao = (x + dx', y + dy', z + dz')
          in if validarMovimento nave novaPosicao
             then do
               putStrLn $ "Movendo a nave " ++ naveId nave ++ " para " ++ show novaPosicao ++ "."
               let novaNave = nave { localizacao = novaPosicao }
               return $ Map.insert idNave novaNave naves
             else do
               putStrLn $ "Movimento inválido para a nave " ++ naveId nave ++ "."
               return naves
        _ -> do
          putStrLn "Instrução 'mover' mal formatada."
          return naves
    Nothing -> do
      putStrLn "Nave não encontrada."
      return naves
processarInstrucao _ naves = do
  putStrLn "Instrução desconhecida."
  return naves

-- Função para carregar naves a partir de um arquivo
carregarNaves :: FilePath -> IO (Map String Nave)
carregarNaves caminho = do
  conteudo <- readFile caminho
  let linhas = lines conteudo
  foldM (flip processarInstrucao) Map.empty (map words linhas)

-- Função para salvar o estado das naves em um arquivo
salvarNaves :: FilePath -> Map String Nave -> IO ()
salvarNaves caminho naves = do
  withFile caminho WriteMode $ \handle -> do
    mapM_ (hPutStrLn handle . show) (Map.elems naves)
  putStrLn "Estado das naves salvo com sucesso."

-- Função principal do menu
menu :: Map String Nave -> IO ()
menu naves = do
  putStrLn "\nMenu:"
  putStrLn "1. Listar todas as naves"
  putStrLn "2. Executar ação em uma nave"
  putStrLn "3. Executar ações em todas as naves"
  putStrLn "4. Salvar estado das naves"
  putStrLn "5. Sair"
  putStr "Escolha uma opção: "
  opcao <- getLine
  case opcao of
    "1" -> do
      mapM_ (putStrLn . show) (Map.elems naves)
      menu naves
    "2" -> do
      putStr "Digite o ID da nave: "
      idNave <- getLine
      instrucao <- getLine
      navesAtualizadas <- processarInstrucao (words instrucao) naves
      menu navesAtualizadas
    "3" -> do
      putStrLn "Executando ações em todas as naves..."
      navesAtualizadas <- foldM (\acc instr -> processarInstrucao instr acc) naves (map words (lines (show naves)))
      menu navesAtualizadas
    "4" -> do
      putStr "Digite o caminho do arquivo para salvar: "
      caminho <- getLine
      salvarNaves caminho naves
      menu naves
    "5" -> putStrLn "Saindo..."
    _ -> do
      putStrLn "Opção inválida."
      menu naves

main :: IO ()
main = do
  putStr "Digite o caminho do arquivo para carregar as naves: "
  caminho <- getLine
  naves <- carregarNaves caminho
  menu naves