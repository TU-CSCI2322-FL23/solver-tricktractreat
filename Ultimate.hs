module Ultimate where 

data Player = X | O deriving (Show, Eq)
data Winner = Champ Player | Tie deriving (Show, Eq)
data SubBoard = InProgress [[Maybe Player]] | Finished Winner deriving (Show, Eq)
type Coord = (Int, Int)
type BigMove = (Coord, Coord) -- first coord is the location of the SUBBOARD second is for the spot on the subboard
type GameState = (Player, Maybe Coord, [[SubBoard]])

findLegalMoves :: GameState -> [BigMove]

-- Finds sub board and returns all legal moves in it. If it is finished, calls function above
findLegalMoves (player, Just subCoords, board) = 
  let subBoard = findSubBoard subCoords board
  in if isInProgress subBoard
     then makeBigMove subCoords (findLegalSubBoardMoves subBoard) 
     else findLegalMoves (player, Nothing, board)

-- When no next coord is specified, finds all in progress boards and returns all legal moves
findLegalMoves (player, Nothing, board) = 
  let labeledValidMoves = [((x, y), findLegalSubBoardMoves subBoard) | (row, x) <- zip board [1..], (subBoard, y) <- zip row [1..], isInProgress subBoard]
  in concat $ map (\(coord, list) -> makeBigMove coord list) labeledValidMoves

-- Finds a sub board given a coord. Isolates row of boards given y coord, then picks the right one based on x coord. 
-- Will likely rewrite
findSubBoard :: Coord -> [[SubBoard]] -> SubBoard
findSubBoard (x, y) bigBoard = 
  let boardRow = head [boardRows | (rowIdx, boardRows) <- (zip [1..] bigBoard), rowIdx == y]
  in head [subBoard | (subIdx, subBoard) <- (zip [1..] boardRow), subIdx == x]

-- Given a sub board, gives coord values to each spot and returns coords of spots that have no plays in them
findLegalSubBoardMoves :: SubBoard -> [Coord]
findLegalSubBoardMoves subBoard = [coord | (coord, play) <- assignCoordinates subBoard, play == Nothing]

-- Helpers
makeBigMove :: Coord -> [Coord] -> [BigMove]
makeBigMove x ys = [(x, y) | y <- ys]

isInProgress :: SubBoard -> Bool
isInProgress (InProgress _) = True
isInProgress _ = False

-- Function to assign coordinate to each spot in a subboard. General use
assignCoordinates :: SubBoard -> [(Coord, Maybe Player)]
assignCoordinates (InProgress lst) = [((x, y), play) | (row, x) <- zip lst [1..], (play, y) <- zip row [1..]]

-- functions for
  -- winner of a game state (GameState -> Winner) Joseph
  -- update game state (GameState -> Move -> GameState) Gaya

updateGameState :: GameState -> BigMove -> GameState
updateGameState game@(p, Just (x, y), boards) move@((outerX, outerY), (innerX, innerY)) =
  if (not $ move `elem` (findLegalMoves game))
  then error "Invalid move"
  else (opponent p, nextMove, nextBoard)
  where finished = not $ isInProgress $ findSubBoard (outerX, outerY) boards
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
  -- pretty show fuction (Game -> String) Blake

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
      showNext = case next of Nothing -> "They may play anywhere."
                              Just x  -> "They must play in sub-board " ++ show x

      showSmallRow i (Finished w) = case w of Champ p -> unwords $ map showCell (replicate 3 (Just p))
                                              Tie     -> "[-] [-] [-]"
      showSmallRow 1 (InProgress [top, mid, bottom]) = unwords $ map showCell top
      showSmallRow 2 (InProgress [top, mid, bottom]) = unwords $ map showCell mid
      showSmallRow 3 (InProgress [top, mid, bottom]) = unwords $ map showCell bottom

      showCell Nothing = "[ ]"
      showCell (Just p) = "[" ++ show p ++ "]"

      showBigRow [ls, cs, rs] = 
          unlines [showSmallRow i ls ++ " | " ++ showSmallRow  i cs ++ " | " ++ showSmallRow i rs | i <- [1..3] ]

      showBoard [] = ""
      showBoard (x:xs) = showBigRow x ++ hline ++ "\n" ++ showBoard xs

  in showTurn ++ "\n" ++ showNext ++ "\n" ++ showBoard board

hline = "------------|-------------|------------"