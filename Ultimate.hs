module Ultimate where
import Data.Maybe
import Data.List

data Player = X | O deriving (Show, Eq)
data Winner = Champ Player | Tie deriving (Show, Eq)
data SubBoard = InProgress [[Maybe Player]] | Finished Winner deriving (Show, Eq)
type Coord = (Int, Int)
type BigMove = (Coord, Coord) -- first coord is the location of the SUBBOARD second is for the spot on the subboard
type GameState = (Player, Maybe Coord, [[SubBoard]])

-- FIND LEGAL MOVES AND HELPERS ETC.
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

-- UPDATE GAME STATE
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

-- CHECK WINNER HELPERS ETC.
winnerB :: GameState -> Maybe Winner
winnerB (p, mc, sbs) =
  case checkWinner (InProgress [hasChamp s | s <- sbs]) of
    Just outcome -> Just outcome
    Nothing -> if or [isInProgress s| s <- concat sbs] then Nothing else Just Tie
  
hasChamp :: [SubBoard] -> [Maybe Player]
hasChamp [] = []
hasChamp (sb:sbs) = [congrad (checkWinner sb)] ++ (hasChamp sbs)


congrad :: Maybe Winner -> Maybe Player
congrad Nothing = Nothing
congrad (Just Tie) = Nothing
congrad (Just (Champ x)) = Just x


  --let
  --winLine = [l |l <-lineLst, (checkLine l) /= Nothing]
  --in if winLine == [] then Nothing else Just (checkLine (head winLine)) 

--missing case for full board tie game. Functionality not confirmed.XX

-- checkLine :: Line -> SubBoard -> Maybe Winner
-- checkLine (ci:ca:co) sb  = undefined
-- if ci == Finished x && ci == ca == co then Just x else Nothing
--fix Logic?XXXXX


checkWinner :: SubBoard -> Maybe Winner
checkWinner (Finished outcome) = Just outcome
checkWinner (InProgress sb) =
  case (checkDia sb, checkRow sb, checkCol sb) of
       (Just x, _, _) -> Just (Champ x)
       (_, Just x, _) -> Just (Champ x)
       (_, _, Just x) -> Just (Champ x) 
       (_, _, _) -> if isFull sb then Just Tie else Nothing

checkDia :: [[Maybe Player]]  -> Maybe Player
checkDia sb = let
  tl = head (head sb)
  tr = last (head sb)
  mid = head (tail (head (tail sb)))
  bl = head (last sb)
  br = last (last sb)
  in if ((tl == mid && mid == br) || (tr == mid && mid == bl)) then mid else Nothing
--(InProgress((i:x:t):(a:xs:ts):(o:xxs:tts))) = if (xs /= Nothing && ((i == xs && (head tts) == (head xs) ) || (o == xs && (head t)
-- = xs))) then xs else Nothing

checkRow :: [[Maybe Player]]  -> Maybe Player
checkRow b = firstJust $ map aux b -- if any (aux X) board then Just X else if any (aux O) board then Just O else Nothing
  where
    aux [Just X, Just X, Just X] = Just X
    aux [Just O,Just O,Just O] = Just O
    aux _ = Nothing

--checkRow [] = Nothing
--checkRow (InProgress(x:xs)) = if (length[p | p <- x, p == X]==3 || length[p | p <- x, p == O]==3) then head x else checkRow xs 

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:vs) = firstJust vs
firstJust (Just v:_)= Just v

-- ==3 means exactly 3 to win 

checkCol :: [[Maybe Player]]  -> Maybe Player
checkCol b = checkRow ((transpose b))
-- checkCol [] = Nothing
-- checkCol (InProgress((i:x):(a:xs):(o:xxs))) = 
--   if i /= Nothing && a == i && o == i then i else checkCol (x:xs:xxs)


--assignCoordinates :: SubBoard -> [(Coord, Maybe Player)]
--assignCoordinates (InProgress lst) = [((x, y), play) | (row, x) <- zip lst [1..], (play, y) <- zip row [1..]]


isFull :: [[Maybe Player]] -> Bool
isFull (p) = not $ any (==Nothing) (concat p) -- if length[s |s <- p, length(catMaybes s)==3]==3 then Finished Tie else InProgress p

--[] = Finished Tie
--isFull (s:sb) = if length (catMaybes s) == 3 then isFull sb else sb

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