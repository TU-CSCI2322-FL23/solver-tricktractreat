module Ultimate where
import Data.Maybe

data Player = X | O deriving (Show, Eq)
data Winner = Champ Player | Tie deriving (Show, Eq)
data SubBoard = InProgress [[Maybe Player]] | Finished Winner deriving (Show, Eq)
type Coord = (Int, Int)
type BigMove = (Coord, Coord) -- first coord is the location of the SUBBOARD second is for the spot on the subboard
type GameState = (Player, Maybe Coord, [[SubBoard]])

findLegalMoves :: GameState -> [BigMove]

-- Finds sub board and returns all legal moves in it. If it is finished, calls function below
findLegalMoves (player, Just subCoords, board) = 
  case findSubBoard subCoords board of 
    Nothing -> error "Not a valid sub-board"
    Just subBoard@(InProgress _) -> allPairs subCoords (openSpaces subBoard) 
    Just (Finished w) -> findLegalMoves (player, Nothing, board)

  -- in if isInProgress subBoard
  --    else findLegalMoves (player, Nothing, board)

-- When no next coord is specified, finds all in progress boards and returns all legal moves
findLegalMoves (player, Nothing, board) = 
  let labeledBoards = concat $ [labelRow x row | (x,row) <- zip [1..] board]
      labelRow x row  = [((x,y), sb) | (y,sb) <- zip [1..] row]
  in concat [allPairs (x,y) (openSpaces sb) | ((x,y), sb) <- labeledBoards]

  -- concat $ map (\(bigY, bigRow) -> legalRowMoves (1, bigY) bigRow) (zip [1..] board)
  -- where legalRowMoves :: Coord -> [SubBoard] -> [BigMove]
  --       legalRowMoves _ [] = []
  --       legalRowMoves (bigX, bigY) (x:rest) = 
  --         let rowBigMoves = allPairs (bigX, bigY) (openSpaces (Just x))
  --         in rowBigMoves ++ legalRowMoves (bigX + 1, bigY) rest

-- Finds a sub board given a coord. Isolates row of boards given y coord, then picks the right one based on x coord. 
-- Will likely rewrite
findSubBoard :: Coord -> [[SubBoard]] -> Maybe SubBoard
findSubBoard (x, y) bigBoard = 
  do boardRow <- safeIndex y bigBoard
     safeIndex x boardRow
  -- let boardRow = head [boardRows | (rowIdx, boardRows) <- (zip [1..] bigBoard), rowIdx == y]
  -- in head [subBoard | (subIdx, subBoard) <- (zip [1..] boardRow), subIdx == x]

safeIndex :: Int -> [a] -> Maybe a
safeIndex idx [x, y, z] = 
  case idx of
    1 -> Just x
    2 -> Just y
    3 -> Just z
    otherwise -> Nothing
safeIndex _ [] = Nothing

findNothingIdx :: Eq a => [Maybe a] -> [Int] -- looks at a row of a subboard
findNothingIdx row = [j | (j, elem) <- zip [1..] row, elem == Nothing]

-- openSpaces :: [[Maybe Player]] -> [Coord] 
openSpaces :: SubBoard -> [Coord] 
openSpaces (InProgress board) = concat [allPairs i (findNothingIdx row) | (i,row) <- zip [1..] board]
openSpaces _ = []

allPairs :: a -> [b] -> [(a,b)]
allPairs single lst = [(single, lstElem) | lstElem <- lst]

isInProgress :: SubBoard -> Bool
isInProgress (InProgress _) = True
isInProgress _ = False

-- functions for
  -- winner of a game state (GameState -> Winner) Joseph
  -- update game state (GameState -> Move -> GameState) Gaya

updateGameState :: GameState -> BigMove -> GameState
updateGameState game@(p, Just (x, y), boards) move@((outerX, outerY), (innerX, innerY)) =
  if (not $ move `elem` (findLegalMoves game))
  then error "Invalid move"
  else (opponent p, nextMove, nextBoard)
  where finished = case findSubBoard (outerX, outerY) boards of 
                   Nothing -> error "you fucked up"
                   Just (InProgress _) -> False
                   otherwise -> True
        nextMove = if finished then Nothing else Just (innerX, innerY)
        nextBoard = changeBoard (outerX, outerY) (innerX, innerY) p boards

opponent X = O 
opponent O = X

-- this could use rewriting w/ zips and pattern matching, so it's a bit ugly atm
-- sorry guys :(
changeBoard :: Coord -> Coord -> Player -> [[SubBoard]] -> [[SubBoard]]
changeBoard (outerX, 1) (innerX, innerY) val (l:ls) = (changeSubBoardRow outerX (innerX, innerY) val l):ls
changeBoard (outerX, outerY) (innerX, innerY) val (l:ls) = l:(changeBoard (outerX, outerY - 1) (innerX, innerY) val ls)

changeSubBoardRow :: Int -> Coord -> Player -> [SubBoard] -> [SubBoard]
changeSubBoardRow 1 (x, y) val (l:ls) = (changeSubBoard (x, y) val l):ls
changeSubBoardRow oX (x, y) val (l:ls) = l:(changeSubBoardRow (oX - 1) (x, y) val ls)

changeSubBoard :: Coord -> Player -> SubBoard -> SubBoard
changeSubBoard (x, y) val (InProgress (l:ls)) = InProgress (aux (x, y) val (l:ls))
  where aux :: Coord -> Player -> [[Maybe Player]] -> [[Maybe Player]]
        aux (x, 1) val (l:ls) = (changeIndex x val l):ls
        aux (x, y) val (l:ls) = l:(aux (x, y - 1) val ls)
-- changeSubBoard (x, y) val (Finished winner) = Finished winner

changeIndex :: Int -> Player -> [Maybe Player] -> [Maybe Player]
changeIndex 1 val (x:xs) = (Just val):xs
changeIndex index val (x:xs) = x:(changeIndex (index - 1) val xs)

  -- legal moves (Game -> [Move]) Jorge
  -- pretty show function (Game -> String) Blake
{-
[ ] [ ] [ ] | [ ] [ ] [ ] | [ ] [ ] [ ]
[ ] [ ] [ ] | [ ] [ ] [ ] | [ ] [ ] [ ]
[ ] [ ] [ ] | [ ] [ ] [ ] | [ ] [ ] [ ]
------------|-------------|------------
[ ] [ ] [ ] | [ ] [ ] [ ] | [ ] [ ] [ ]
[ ] [ ] [ ] | [ ] [ ] [ ] | [ ] [ ] [ ]
[ ] [ ] [ ] | [ ] [ ] [ ] | [ ] [ ] [ ]
------------|-------------|------------
[ ] [ ] [ ] | [ ] [ ] [ ] | [ ] [ ] [ ]
[ ] [ ] [ ] | [ ] [ ] [ ] | [ ] [ ] [ ]
[ ] [ ] [ ] | [ ] [ ] [ ] | [ ] [ ] [ ]
-}

prettyPrint :: GameState -> String
prettyPrint (player, next, board) =
  let showTurn = "It is " ++ show player ++ "'s turn."
      showNext Nothing = "They may play anywhere."
      showNext (Just x) = "They must play in sub-board " ++ show x

      showSmallRow i (Finished w) = case w of Champ p -> unwords $ replicate 3 (showCell (Just p))
                                              Tie     -> "[-] [-] [-]"
      showSmallRow 1 (InProgress [top, mid, bottom]) = unwords $ map showCell top
      showSmallRow 2 (InProgress [top, mid, bottom]) = unwords $ map showCell mid
      showSmallRow 3 (InProgress [top, mid, bottom]) = unwords $ map showCell bottom
      showSmallRow _ _ = error "Error showing line, possibly corrupted GameState."

      showCell Nothing = "[ ]"
      showCell (Just p) = "[" ++ show p ++ "]"

      showBigRow [ls, cs, rs] = 
          unlines [showSmallRow i ls ++ " | " ++ showSmallRow  i cs ++ " | " ++ showSmallRow i rs | i <- [1..3] ]
      showBigRow _ = error "Error showing \"big row.\" Row does not contain exactly 3 elements, likely corrupted GameState."

      showBoard [] = ""
      showBoard (x:xs) = showBigRow x ++ hline ++ "\n" ++ showBoard xs

  in showTurn ++ "\n" ++ showNext next ++ "\n" ++ showBoard board

hline = "------------|-------------|------------"