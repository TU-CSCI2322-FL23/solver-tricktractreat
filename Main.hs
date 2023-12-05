module Main where

import Ultimate
import Data.List
import Data.Char
import System.IO
import Debug.Trace
import System.Environment
import System.Console.GetOpt

data Flag = Win | Depth String | Help | Move String | Verbose | Interactive deriving (Show, Eq)

options :: [OptDescr Flag]
options = [Option ['h'] ["help"] (NoArg Help) "Print usage information and exit.",
           Option ['w'] ["winner"] (NoArg Win) "Prints the best move with no cut-off depth.",
           Option ['d'] ["depth"] (ReqArg Depth "<num>") "Prints the best move with a specified cut-off depth.",
           Option ['m'] ["move"] (ReqArg Move "<move>") "Makes the given move and prints the resulting board. Moves are two coordinates, entered in the following format: (a,b)(c,d).",
           Option ['v'] ["verbose"] (NoArg Verbose) "Prints more detail (for flags that print moves, using this flag will output the move and a description and for flags that print the next game state, this flag will print the board visually as well."]


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

bestMove :: GameState -> Maybe BigMove
bestMove game = Just ((1, 1), (1, 1))

writeGame :: GameState -> FilePath -> IO ()
writeGame game file =
  do writeFile file (showGame game)

loadGame :: FilePath -> IO GameState
loadGame file =
  do contents <- readFile file
     let gameState = readGame contents
     return gameState

whoMightWin :: GameState -> Int -> Maybe Winner
whoMightWin game depth = Just (Champ X)

boundedBestMove :: GameState -> Int -> Maybe BigMove
boundedBestMove game depth = Just ((1, 1), (1, 1))

--

putBestMove :: GameState -> Bool -> Maybe Int -> IO ()
-- that computes the best move and prints it to standard output
-- print the outcome that moves forces for full credit
putBestMove game verbose depth =
  do let nextMove = case depth of
                      Nothing -> bestMove game
                      Just d -> boundedBestMove game d
         nextGame = case nextMove of
                      Just smth -> updateGameState game smth
                      Nothing -> Nothing
         gameStr = case (nextGame, verbose) of
                     (Nothing, _) -> "no best move"
                     (Just smth, True) -> prettyPrint smth
                     (Just smth, False) -> show smth
     putStr $ "The best move is " ++ show nextMove
     if verbose
     then do let winner :: Maybe Winner
                 winner = case nextGame of
                            Nothing -> Nothing
                            Just ng -> case depth of
                                         Nothing -> Just $ whoWillWin ng
                                         Just smth -> whoMightWin ng smth 
                 winnerStr = case winner of
                               Just (Champ X) -> "win for X"
                               Just (Champ O) -> "win for O"
                               Just Tie -> "tie"
                               Nothing -> "No moves, no winner."
             putStrLn $ " resulting in a " ++ winnerStr
     else do putStr "\n"
     putStr $ "The move results in the following game\n" ++ gameStr ++ "\n"

putMove :: GameState -> Bool -> BigMove -> IO ()
putMove game verbose move =
  do let nextGame = updateGameState game move
         gameStr = case (nextGame, verbose) of
                        (Nothing, _) -> "invalid move entered"
                        (Just smth, True) -> prettyPrint smth
                        (Just smth, False) -> show smth
     putStrLn $ "The move entered results in the following game state:\n" ++ gameStr

main :: IO ()
-- reads a file name from standard input or the arguments, loads the game, and prints the best move
main =
  do args <- getArgs
     let (flags, inputs, errors) = trace (show (getOpt Permute options args)) getOpt Permute options args
         verbose = (Verbose `elem` flags)
         depth = getDepth flags
         move = getMove flags
     -- data Flag = Win | Depth String | Help | Move String | Verbose | Interactive deriving (Show, Eq)
     if Help `elem` flags || not (null errors)
     then putStrLn $ usageInfo "Interactive Ultimate Tic-Tac-Toe app." options ++ "\nThe coordinates for the board are as follows:\n[(1,1)][(2,1)][(3,1)]\n[(1,2)][(2,2)][(3,2)]\n[(1,3)][(2,3)][(3,3)]"
     else do let file = head inputs
             game <- loadGame file
             if null flags || flags == [Verbose]
             then putBestMove game verbose (Just 5) -- default behavior
             --else if Interactive `elem` flags
             --then do play game verbose (getDepth flags)
             else if Win `elem` flags
             then do putBestMove game verbose Nothing
             else if depth /= Nothing
             then putBestMove game verbose (getDepth flags)
             else case move of
                    Just smth -> putMove game verbose smth
                    Nothing -> putStrLn ""

{-
play :: GameState -> Bool -> Maybe Int -> IO ()
play game verbose depth = 
-}

getDepth :: [Flag] -> Maybe Int
getDepth [] = Nothing
getDepth (Depth x:fs) = read x
getDepth (f:fs) = getDepth fs

getMove :: [Flag] -> Maybe BigMove
getMove [] = Nothing
getMove (Move x:fs) = readMove x

readMove :: String -> Maybe BigMove
readMove str = recouple ints
  where list = filter (\x -> x /= '(' && x /= ')' && x /= ',') str
        ints = toInt list

toInt :: String -> [Int]
toInt lst = map digitToInt lst

recouple :: [Int] -> Maybe BigMove
recouple lst =
  case (a, b) of
    ([x, y], [a, b]) -> Just ((x, y), (a, b))
    _ -> Nothing            
  where (a, b) = splitAt 2 lst



