{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Use foldl" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
verifica_embates _ [] = False -- Caso base: sem outros estados para comparar, sem colisão
verifica_embates (coord, _) ((loc, _) : estados) -- Se o estado atual colide com algum outro, retorna True
    | compara_coord coord loc = True
    | otherwise = verifica_embates (coord, False) estados

-- Função auxiliar para extrair estados de [(Estado, String)]
extract_estados :: [(Estado, String)] -> [Estado]
extract_estados [] = [] -- Caso base
extract_estados ((estado, _) : resto) = estado : extract_estados resto -- Extrai cada estado da lista de pares

-- Função auxiliar que verifica se as listas têm o mesmo tamanho
mesmo_tamanho :: [a] -> [b] -> Bool
mesmo_tamanho [] [] = True -- Ambas as listas vazias, então têm o mesmo tamanho
mesmo_tamanho (_ : xs) (_ : ys) = mesmo_tamanho xs ys -- Verifica recursivamente cada elemento
mesmo_tamanho _ _ = False -- Se uma lista é vazia e a outra não, retorna False

-- 6. Versão atualizada de move_varios que evita embates
move_varios_atualizado :: [Nave] -> [Estado] -> [(Estado, String)]
move_varios_atualizado [] _ = [] -- Caso base: sem naves para mover
move_varios_atualizado _ [] = [] -- Caso base: sem estados fornecidos
move_varios_atualizado naves estados
    | not (mesmo_tamanho naves estados) = [] -- Se listas têm tamanhos diferentes, retorna lista vazia
    | otherwise =
        let processar_movimentos = processa_movimentos naves estados -- Calcula os novos estados aplicando movimentos
            novos_estados = extract_estados processar_movimentos -- Extrai os novos estados para verificação de colisão
            havera_colisao = verifica_colisoes novos_estados -- Verifica colisões
         in if havera_colisao
                then manter_estados_originais naves estados -- Se houver colisão, mantém estados antigos
                else processar_movimentos -- Se não houver colisão, usa os novos estados
  where
    -- Função auxiliar que aplica movimentos a cada nave e seu estado original
    processa_movimentos :: [Nave] -> [Estado] -> [(Estado, String)]
    processa_movimentos [] [] = []
    processa_movimentos ((movs, id) : ns) (est : es) = (move_lista movs est, id) : processa_movimentos ns es
    processa_movimentos _ _ = [] -- Caso listas tenham tamanhos diferentes, retorna lista vazia

    -- Função auxiliar que mantém os estados originais se houver colisão
    manter_estados_originais :: [Nave] -> [Estado] -> [(Estado, String)]
    manter_estados_originais [] [] = []
    manter_estados_originais ((_, id) : ns) (est : es) = (est, id) : manter_estados_originais ns es
    manter_estados_originais _ _ = [] -- Caso listas tenham tamanhos diferentes, retorna lista vazia

    -- Extrai IDs de uma lista de Naves
    extrair_ids :: [Nave] -> [String]
    extrair_ids [] = []
    extrair_ids ((_, id) : ns) = id : extrair_ids ns

    -- Verifica colisões entre estados
    verifica_colisoes :: [Estado] -> Bool
    verifica_colisoes [] = False
    verifica_colisoes (x : xs) = verifica_embates x xs || verifica_colisoes xs
