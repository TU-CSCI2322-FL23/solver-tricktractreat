module Main where

import Ultimate
import System.IO
import Debug.Trace
import System.Environment
import System.Console.GetOpt

{-
-- dummy functions, required for the IO functions to work
readGame :: String -> GameState
readGame string = defaultGame

row = [Just X, Just X, Just X]
diffRow = [Nothing, Just X, Just X]
sb = InProgress [diffRow, row, row]
bRow = [sb, sb, sb]
board = [bRow, bRow, bRow]
defaultGame = (X, Just (1, 1), board)

showGame :: GameState -> String
showGame game = "Show!!"

bestMove :: GameState -> BigMove
bestMove game = ((1, 1), (1, 1))

whoWillWin game = Champ X
-}

writeGame :: GameState -> FilePath -> IO ()
writeGame game file =
  do writeFile file (showGame game)

loadGame :: FilePath -> IO GameState
loadGame file =
  do contents <- readFile file
     let gameState = readGame contents
     return gameState

putBestMove :: GameState -> IO ()
-- that computes the best move and prints it to standard output
-- print the outcome that moves forces for full credit
putBestMove game =
  do let nextMove = bestMove game
     let winner = case whoWillWin game of
                    Champ X -> "win for X"
                    Champ O -> "win for O"
                    Tie -> "tie"
     let nextGame = updateGameState game nextMove
     let gameStr = case nextGame of
                    Nothing -> "no best move"
                    Just smth -> prettyPrint smth
     putStr $ "The best move is " ++ show nextMove ++ " resulting in a " ++ winner ++ "\n"
     putStr $ "The move results in the following game\n" ++ gameStr ++ "\n"

main :: IO ()
-- reads a file name from standard input or the arguments, loads the game, and prints the best move
main =
  do args <- getArgs
     let file = case args of
                 (x:xs) -> x
                 _ -> "test.txt"
     game <- loadGame file
     putBestMove game


