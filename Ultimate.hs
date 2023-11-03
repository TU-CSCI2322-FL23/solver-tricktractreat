data Player = X | O deriving (Show, Eq)
data Winner = Champ Player | Tie deriving (Show, Eq)
data SubBoard = InProgress [[Maybe Player]] | Finished Winner deriving (Show, Eq)
type Coord = (Int, Int)
type BigMove = (Coord, Coord) -- first coord is the location of the SUBBOARD second is for the spot on the subboard
type GameState = (Player, Maybe Coord, [[SubBoard]])
-- findlegalmoves branch

--e.g. findLegalMoves (X, Just (2,3), ...)


-- findLegalMoves :: GameState -> [BigMove]
findLegalMoves :: GameState -> SubBoard
findLegalMoves (player, Just (x, y), board) = 
  let rows = head [boardRows | (rowIdx, boardRows) <- (zip [1..] board), rowIdx == y]
      subBoard = head [subBoard | (subIdx, subBoard) <- (zip [1..] rows), subIdx == x]
  in subBoard 
  -- in findLegalSubBoardMoves player subBoard

-- findLegalSubBoardMoves :: Player -> SubBoard -> [Coord]
findLegalSubBoardMoves :: Player -> SubBoard -> String
findLegalSubBoardMoves player subBoard@(InProgress lst) = undefined
  -- map (\x -> map show x) lst



-- x = map (\tup -> ((3,2), tup)) [(1,3), (2,2), (2,1)] -- Making BigMove 
y = InProgress [[Just X, Just O, Just X], [Just O, Nothing, Just X]] -- SubBoard example

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