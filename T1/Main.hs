module Main where

import Comandos

-- Função auxiliar para imprimir resultados dos testes
printResultado :: (Show a, Eq a) => String -> a -> a -> IO ()
printResultado nomeTeste resultado esperado = do
    putStrLn $ nomeTeste ++ ": " ++ if resultado == esperado then "Passou" else "Falhou"
    putStrLn $ "  Resultado: " ++ show resultado
    putStrLn $ "  Esperado: " ++ show esperado

-- Testes para a função atualiza_acao
testAtualizaAcao :: IO ()
testAtualizaAcao = do
    let estadoInicial = ((0, 0, 0), False)
    let estadoEsperado1 = ((0, 0, 0), True)
    let estadoEsperado2 = ((0, 0, 0), False)
    printResultado "Testa ligar" (atualiza_acao ("ligar", estadoInicial) estadoInicial) estadoEsperado1
    printResultado "Testa desligar" (atualiza_acao ("desligar", estadoInicial) estadoInicial) estadoEsperado2

-- Testes para a função move
testMove :: IO ()
testMove = do
    let estadoInicial = ((0, 0, 0), True)
    let movimento = (1, 1, 1)
    let estadoEsperado = ((1, 1, 1), True)
    printResultado "Testa movimento com nave ligada" (move movimento estadoInicial) estadoEsperado
    let estadoDesligado = ((0, 0, 0), False)
    printResultado "Testa movimento com nave desligada" (move movimento estadoDesligado) estadoDesligado

-- Testes para a função move_lista
testMoveLista :: IO ()
testMoveLista = do
    let estadoInicial = ((0, 0, 0), True)
    let movimentos = [(1, 1, 1), (2, 2, 2)]
    let estadoEsperado = ((3, 3, 3), True)
    printResultado "Testa lista de movimentos" (move_lista movimentos estadoInicial) estadoEsperado

-- Testes para a função move_varios
testMoveVarios :: IO ()
testMoveVarios = do
    let naves = [([(1, 1, 1)], "Nave1"), ([(2, 2, 2)], "Nave2")]
    let estados = [((0, 0, 0), True), ((0, 0, 0), True)]
    let estadosEsperados = [(((1, 1, 1), True), "Nave1"), (((2, 2, 2), True), "Nave2")]
    printResultado "Testa mover várias naves" (move_varios naves estados) estadosEsperados

-- Testes para a função verifica_embates
testVerificaEmbates :: IO ()
testVerificaEmbates = do
    let estado = ((0, 0, 0), True)
    let estados = [((0, 0, 0), True), ((1, 1, 1), True)]
    printResultado "Testa embate verdadeiro" (verifica_embates estado estados) True
    let estadosSemEmbate = [((1, 1, 1), True), ((2, 2, 2), True)]
    printResultado "Testa embate falso" (verifica_embates estado estadosSemEmbate) False

-- Testes para a função move_varios_atualizado
testMoveVariosAtualizado :: IO ()
testMoveVariosAtualizado = do
    let naves = [([(1, 1, 1)], "Nave1"), ([(1, 1, 1)], "Nave2")]
    let estados = [((0, 0, 0), True), ((1, 1, 1), True)]
    let estadosEsperados = [(((1, 1, 1), True), "Nave1"), (((2, 2, 2), True), "Nave2")]
    printResultado "Testa mover várias naves sem colisão" (move_varios_atualizado naves estados) estadosEsperados
    let navesComColisao = [([(1, 1, 1)], "Nave1"), ([(0, 0, 0)], "Nave2")]
    let estadosEsperadosComColisao = [(((0, 0, 0), True), "Nave1"), (((1, 1, 1), True), "Nave2")]
    printResultado "Testa mover várias naves com colisão" (move_varios_atualizado navesComColisao estados) estadosEsperadosComColisao

-- Função principal para executar todos os testes
main :: IO ()
main = do
    testAtualizaAcao
    testMove
    testMoveLista
    testMoveVarios
    testVerificaEmbates
    testMoveVariosAtualizado
