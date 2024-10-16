{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Use foldl" #-}

module Comandos where

-- localização -> (x, y, z) -> (Float,Float,Float)
-- estado -> (Localização, ligado) -> ((Float,Float,Float), Bool)
-- movimento -> (dx, dy, dz) -> (Float,Float,Float)

atualiza_acao :: (Bool, ((Float, Float, Float), Bool)) -> ((Float, Float, Float), Bool)
atualiza_acao (ligado, (coord, _))
    | ligado == True = (coord, True)
    | ligado == False = (coord, False)
    | otherwise = (coord, False) -- Se a ação for inválida

move :: (Float, Float, Float) -> ((Float, Float, Float), Bool) -> ((Float, Float, Float), Bool)
move (dx, dy, dz) ((x, y, z), ligado)
    | ligado == True = ((x + dx, y + dy, z + dz), ligado)
    | otherwise = ((x, y, z), ligado) -- A nave não se move se estiver desligada

move_lista :: [(Float, Float, Float)] -> ((Float, Float, Float), Bool) -> ((Float, Float, Float), Bool)
move_lista [] estado = estado
move_lista (m : ms) estado = move_lista ms (move m estado)