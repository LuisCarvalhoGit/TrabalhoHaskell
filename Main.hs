module Main where

import Comandos

-- Test helper function to display results
runTest :: String -> Bool -> IO ()
runTest name result = putStrLn $ name ++ ": " ++ if result then "PASSED" else "FAILED"

-- Test Cases for move_varios_atualizado
testMoveVariosAtualizado :: IO ()
testMoveVariosAtualizado = do
    -- Test 1: Basic movement without collision
    let nave1 = ([(1,1,1), (2,2,2)], "nave1")
    let nave2 = ([(1,0,0), (0,1,0)], "nave2")
    let estado1 = ((0,0,0), True)
    let estado2 = ((0,0,0), True)
    let result1 = move_varios_atualizado [nave1, nave2] [estado1, estado2]
    let expected1 = [(((3,3,3), True), "nave1"), (((1,1,0), True), "nave2")]
    runTest "Move Varios Atualizado - Basic Movement" (result1 == expected1)

    -- Test 2: Collision detection
    let nave3 = ([(1,1,1)], "nave3")
    let nave4 = ([(1,1,1)], "nave4")
    let estado3 = ((0,0,0), True)
    let estado4 = ((1,1,1), True)
    let result2 = move_varios_atualizado [nave3, nave4] [estado3, estado4]
    let expected2 = [(((0,0,0), True), "nave3"), (((1,1,1), True), "nave4")]
    runTest "Move Varios Atualizado - Collision Detection" (result2 == expected2)

    -- Test 3: Empty list of naves
    let result3 = move_varios_atualizado [] [estado1, estado2]
    let expected3 = []
    runTest "Move Varios Atualizado - Empty Naves List" (result3 == expected3)

    -- Test 4: Empty list of estados
    let result4 = move_varios_atualizado [nave1, nave2] []
    let expected4 = []
    runTest "Move Varios Atualizado - Empty Estados List" (result4 == expected4)

    -- Test 5: Different lengths of input lists
    let result5 = move_varios_atualizado [nave1] [estado1, estado2]
    let expected5 = []
    runTest "Move Varios Atualizado - Different Lengths" (result5 == expected5)

    -- Test 6: Multiple naves with no collisions
    let nave5 = ([(1,1,1)], "nave5")
    let nave6 = ([(2,2,2)], "nave6")
    let estado5 = ((0,0,0), True)
    let estado6 = ((0,0,0), True)
    let result6 = move_varios_atualizado [nave5, nave6] [estado5, estado6]
    let expected6 = [(((1,1,1), True), "nave5"), (((2,2,2), True), "nave6")]
    runTest "Move Varios Atualizado - Multiple Naves No Collisions" (result6 == expected6)

-- Main function to run all tests
main :: IO ()
main = do
    putStrLn "Running tests..."
    testMoveVariosAtualizado
    putStrLn "Tests completed."
