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
type Nave = ([Movimento], String)  -- Lista de movimentos e ID da nave
type ListaNaves = [(Nave, Estado)] -- Lista de pares de (Nave, Estado) 

atualiza_acao :: (Bool, Estado) -> Estado
atualiza_acao (acao, (coord, _)) = (coord, acao)


move :: Movimento -> Estado -> Estado
move (dx, dy, dz) ((x, y, z), ligado)
    | ligado == True = ((x + dx, y + dy, z + dz), ligado)
    | otherwise = ((x, y, z), ligado) -- A nave não se move se estiver desligada


move_lista :: [Movimento] -> Estado -> Estado
move_lista [] estado = estado
move_lista (m : ms) estado = move_lista ms (move m estado)


move_varios :: [Nave] -> [Estado] -> [(Estado, String)]
move_varios [] [] = [] -- Caso base
move_varios ((movs, id):naves) (estado:estados) = (move_lista movs estado, id) : move_varios naves estados
move_varios _ _ = [] -- Listas com tamanhos diferentes


-- 5. Função que verifica embates entre naves
verifica_embates :: Estado -> [Estado] -> Bool
verifica_embates (novaLocalizacao, _) = any (\(localizacao, _) -> localizacao == novaLocalizacao)


--  6.Crie uma nova versão, atualizada, da função move_varios que apenas move uma nave caso esse movimento não origine um embate.

move_varios_atualizado :: [Nave] -> [Estado] -> [(Estado, String)]
move_varios_atualizado [] [] = [] -- Caso base
move_varios_atualizado ((movs, id):naves) (estado:estados) = 
    let novoEstado = move_lista movs estado
        embate = verifica_embates (fst novoEstado) (map fst (naves ++ estados))
    in if embate
       then (estado, id) : move_varios naves estados -- Não move a nave se houver embate
       else (novoEstado, id) : move_varios naves estados
move_varios_atualizado _ _ = [] -- Listas com tamanhos diferentes


