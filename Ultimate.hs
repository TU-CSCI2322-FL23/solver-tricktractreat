module Ultimate where 

data Player = X | O deriving (Show, Eq)
data Winner = Champ Player | Tie deriving (Show, Eq)
data SubBoard = InProgress [[Maybe Player]] | Finished Winner deriving (Show, Eq)
type Coord = (Int, Int)
type BigMove = (Coord, Coord) -- first coord is the location of the SUBBOARD second is for the spot on the subboard
type GameState = (Player, Maybe Coord, [[SubBoard]])

findLegalMoves :: GameState -> [BigMove]

-- When no next coord is specified, finds all in progress boards and returns all legal moves
findLegalMoves (player, Nothing, board) = 
  let labeledValidMoves = [((x, y), findLegalSubBoardMoves subBoard) | (row, x) <- zip board [1..], (subBoard, y) <- zip row [1..], isInProgress subBoard]
  in concat $ map (\(coord, list) -> makeBigMove coord list) labeledValidMoves

-- Finds sub board and returns all legal moves in it. If it is finished, calls function above
findLegalMoves (player, Just subCoords, board) = 
  let subBoard = findSubBoard subCoords board
  in case subBoard of 
    InProgress _ -> makeBigMove subCoords (findLegalSubBoardMoves subBoard) 
    Finished _ -> findLegalMoves (player, Nothing, board)

makeBigMove :: Coord -> [Coord] -> [BigMove]
makeBigMove x ys = [(x, y) | y <- ys]

isInProgress :: SubBoard -> Bool
isInProgress (InProgress _) = True
isInProgress _ = False

-- Finds a sub board given a coord. Isolates row of boards given y coord, then picks the right one based on x coord. 
-- Will likely rewrite
findSubBoard :: Coord -> [[SubBoard]] -> SubBoard
findSubBoard (x, y) bigBoard = 
  let boardRow = head [boardRows | (rowIdx, boardRows) <- (zip [1..] bigBoard), rowIdx == y]
  in head [subBoard | (subIdx, subBoard) <- (zip [1..] boardRow), subIdx == x]

-- Given a sub board, gives coord values to each spot and returns coords of spots that have no plays in them
findLegalSubBoardMoves :: SubBoard -> [Coord]
findLegalSubBoardMoves subBoard = [coord | (coord, play) <- assignCoordinates subBoard, play == Nothing]

-- Function to assign coordinate to each spot in a subboard. General use
assignCoordinates :: SubBoard -> [(Coord, Maybe Player)]
assignCoordinates (InProgress lst) = [((x, y), play) | (row, x) <- zip lst [1..], (play, y) <- zip row [1..]]

-- functions for
  -- winner of a game state (GameState -> Winner) Joseph
  -- update game state (GameState -> Move -> GameState) Gaya
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