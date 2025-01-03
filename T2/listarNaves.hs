-- 1. Função para listar todas as naves
listarNaves :: [Nave] -> IO ()
listarNaves naves = do
    putStrLn "\n=== Lista de naves ===\n"
    mapM_ imprimirNaves naves

-- 1.1 Imprimir o estado da nave
imprimirNaves :: Nave -> IO ()
imprimirNaves nave = do
    putStrLn "--- Estado Atual da Nave --- "
    putStrLn $ "ID: " ++ naveID nave
    putStrLn $ "Posição: " ++ show (position nave)
    putStrLn $ "Estado: " ++ show (state nave)
    putStrLn $ "Espaço Permitido: " ++ show (limits nave)
    putStrLn ""
