import Data.Maybe

data Player = X | O deriving (Show, Eq)
data Winner = Champ Player | Tie deriving (Show, Eq)
data SubBoard = InProgress [[Maybe Player]] | Finished Winner deriving (Show, Eq)
type Coord = (Int, Int)
type BigMove = (Coord, Coord) -- first coord is the location of the SUBBOARD second is for the spot on the subboard
type GameState = (Player, Maybe Coord, [[SubBoard]])
type Line = [Coord] --added line typeXX

--list of list of cordinates that form winning linesXX
lineLst = [[(1,1),(2,2),(3,3)],[(3,1),(2,2),(1,3)],[(1,1),(1,2),(1,3)],
           [(2,1),(2,2),(2,3)],[(3,1),(3,2),(3,3)],[(1,1),(2,1),(3,1)],[(1,2),(2,2),(3,2)],[(1,3),(2,3),(3,3)]]

testLine = [(1,1),(1,2),(1,3)]
testSubBoard1_1 = InProgress [[Just X, Nothing, Just O], [Just X, Just X, Just X], [Nothing, Nothing, Just O]]
testSubBoard2_1 = InProgress [[Just O, Nothing, Nothing], [Just X, Just X, Nothing], [Nothing, Nothing, Nothing]]
testSubBoard3_1 = InProgress [[Just X, Just O, Nothing], [Nothing, Nothing, Nothing], [Just O, Just O, Nothing]]
testSubBoard1_2 = InProgress [[Nothing, Just X, Just X], [Nothing, Just O, Nothing], [Nothing, Nothing, Nothing]]
testSubBoard3_2 = InProgress [[Just X, Just O, Just X], [Just O, Just X, Just X], [Just O, Just X, Just O]]

mp3 = [[Just X, Just O, Just X], [Just O, Just X, Just X], [Just O, Just X, Just O]]
mp4 = [[Just X, Just O, Just X], [Just O, Nothing, Just X], [Just O, Just X, Just O]]
--testSubBoard2_3 = Finished Champ X
-- GameState should be a type, not a variable type def

-- functions for
  -- winner of a game state (GameState -> Winner) Joseph
  -- update game state (GameState -> Move -> GameState) Gaya
  -- legal moves (Game -> [Move]) Jorge
  -- pretty show fuction (Game -> String) Blake
isInProgress :: SubBoard -> Bool
isInProgress (InProgress _) = True
isInProgress _ = False

winnerB :: GameState -> Maybe Winner
winnerB (p, mc, sbs) = undefined
  
  --let
  --winLine = [l |l <-lineLst, (checkLine l) /= Nothing]
  --in if winLine == [] then Nothing else Just (checkLine (head winLine)) 

--missing case for full board tie game. Functionality not confirmed.XX

--checkLine :: Line -> SubBoard -> Maybe Winner
--checkLine (ci:ca:co) sb  = undefined
--if ci == Finished x && ci == ca == co then Just x else Nothing
--fix Logic?XXXXX

-- miniWinner :: SubBoard -> SubBoard
-- miniWinner sb = 
--   case sb of
--     Finished sb -> sb 
--     --checkWinner inProgress sb /= Nothing -> Just checkWinner inProgress sb
--     isFull inProgress sb -> isFull inProgress sb


--  | map
--  | isFull sb = Tie
--  | otherwise = sb
-- else sb
--miniWinner sb = let
  --winLine = [l |l <-lineLst, (checkLine l) /= Nothing]
  --if winLine  =
isFull :: [[Maybe Player]] -> SubBoard
isFull sb = if length[s |s <- sb, length(catMaybes s)==3]==3 then Finished Tie else InProgress sb

--[] = Finished Tie
--isFull (s:sb) = if length (catMaybes s) == 3 then isFull sb else sb

--map lookup (-1?)
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
