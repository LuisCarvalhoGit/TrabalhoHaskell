{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Use foldl" #-}

module Comandos where

-- localização -> (x, y, z) -> (Float,Float,Float)
-- estado -> (Localização, ligado) -> ((Float,Float,Float), Bool)
-- movimento -> (dx, dy, dz) -> (Float,Float,Float)

-- Tipos Sinónimos
type Coord = (Float, Float, Float)
type Estado = (Coord, Bool)
type Movimento = Coord
--type Nave = ([Movimento], String)  -- Lista de movimentos e 
--type ListaNaves = [(Nave, Estado)] -- List of (ship, initial state) pairs

atualiza_acao :: (Bool, Estado) -> Estado
atualiza_acao (acao, (coord, _)) = (coord, acao)
    

move :: Movimento -> Estado -> Estado
move (dx, dy, dz) ((x, y, z), ligado)
    | ligado == True = ((x + dx, y + dy, z + dz), ligado)
    | otherwise = ((x, y, z), ligado) -- A nave não se move se estiver desligada


move_lista :: [Movimento] -> Estado -> Estado
move_lista [] estado = estado
move_lista (m : ms) estado = move_lista ms (move m estado)