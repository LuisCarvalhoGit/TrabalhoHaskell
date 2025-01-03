import Control.Exception
import Control.Monad
import Data.List
import Distribution.Utils.Path (sameDirectory)
import System.Directory
import System.IO

-- Types
type Coordinates = (Int, Int, Int) -- Coordenadas de uma nave
type ShipState = (Coordinates, Bool) -- localização, estado (ligado/desligado)
type SpaceLimits = (Coordinates, Coordinates) -- ((min_x, min_y, min_z), (max_x, max_y, max_z))

data Ship = Ship
    { shipId :: String
    , state :: ShipState
    , limits :: SpaceLimits
    , position :: Coordinates
    }
    deriving (Show)

-- menu principal
menuPrincipal :: IO String
menuPrincipal = do
    let menutxt =
            unlines
                [ "\n=== Menu de Controlo ==="
                , "\t1. Listar todas as naves"
                , "\t2. Executar acoes para uma nave"
                , "\t3. Executar acoes para todas as naves"
                , "\t4. Acao por input direto"
                , "\t5. Sair"
                ]

    hSetBuffering stdout NoBuffering -- from System.IO
    putStrLn menutxt
    putStrLn "Escolha uma opção: "
    getLine

-- Mostar menu
showMenu :: IO ()
showMenu = do
    escolha <- menuPrincipal
    case escolha of
        "1" -> do listarNaves
        "2" -> do execNave
        "3" -> do execTodas
        "4" -> do inputDireto
        "5" -> do sair
        otherwise -> do erro escolha

-- Error handling
erro :: String -> IO ()
erro escolha =
    putStrLn $ "A opção " ++ escolha ++ "é inválida!"
