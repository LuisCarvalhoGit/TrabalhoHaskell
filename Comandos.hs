{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Use foldl" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Comandos where

-- Tipos Sinónimos
type Coord = (Float, Float, Float) -- Representa a localização (x, y, z)
type Estado = (Coord, Bool) -- Representa (localização, ligado)
type Movimento = Coord -- Representa (dx, dy, dz)
type Acao = (String, Estado) -- Representa a ação (ligar/desligar)
type Nave = ([Movimento], String) -- Lista de movimentos e ID da nave

-- 1. Função que atualiza o estado baseado em uma ação
atualiza_acao :: Acao -> Estado -> Estado
atualiza_acao (acao, _) (coord, _)
    | acao == "ligar" = (coord, True)
    | acao == "desligar" = (coord, False)
    | otherwise = (coord, False)

-- 2. Função que move a nave se estiver ligada
move :: Movimento -> Estado -> Estado
move (dx, dy, dz) ((x, y, z), ligado)
    | ligado == True = ((x + dx, y + dy, z + dz), ligado)
    | otherwise = ((x, y, z), ligado) -- A nave não se move se estiver desligada

-- 3. Função que aplica uma lista de movimentos
move_lista :: [Movimento] -> Estado -> Estado
move_lista [] estado = estado
move_lista (m : ms) estado = move_lista ms (move m estado)

-- 4. Função que move várias naves
move_varios :: [Nave] -> [Estado] -> [(Estado, String)]
move_varios [] [] = [] -- Caso base
move_varios ((movs, id) : naves) (estado : estados) = (move_lista movs estado, id) : move_varios naves estados
move_varios _ _ = [] -- Listas com tamanhos diferentes

-- Função auxiliar que compara duas localizações
compara_coord :: Coord -> Coord -> Bool
compara_coord (x1, y1, z1) (x2, y2, z2) = x1 == x2 && y1 == y2 && z1 == z2

-- 5. Função que verifica embates entre naves
verifica_embates :: Estado -> [Estado] -> Bool
verifica_embates _ [] = False
verifica_embates (loc1, _) ((loc2, _) : estados)
    | compara_coord loc1 loc2 = True
    | otherwise = verifica_embates (loc1, False) estados

-- 6. Versão atualizada de move_varios que evita embates

move_varios_atualizado :: [Nave] -> [Estado] -> [(Estado, String)]
move_varios_atualizado [] _ = [] -- Caso lista de Naves vazia
move_varios_atualizado _ [] = [] -- Caso lista de Estados vazia
move_varios_atualizado ((movs, id) : naves) (estado : estados) =
    let resultadoPrevio = move_varios_atualizado naves estados -- Processa o restante das naves recursivamente
        estadosFinais = map fst resultadoPrevio -- Extrai os estados finais das naves após se moverem
        novoEstado = move_lista movs estado
    in if verifica_embates novoEstado estadosFinais
        then (estado, id) : resultadoPrevio -- Não move a nave se houver colisão
        else (novoEstado, id) : resultadoPrevio -- Move a nave se não houver colisão
move_varios_atualizado _ _ = [] -- Caso de listas com diferentes tamanhos
