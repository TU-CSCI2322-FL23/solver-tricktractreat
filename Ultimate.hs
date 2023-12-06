module Ultimate where
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Char (digitToInt)
import Debug.Trace

data Player = X | O deriving (Show, Eq)
data Winner = Champ Player | Tie deriving (Show, Eq)
data SubBoard = InProgress [[Maybe Player]] | Finished Winner deriving (Show, Eq)
type Coord = (Int, Int)
type BigMove = (Coord, Coord) -- first coord is the location of the SUBBOARD second is for the spot on the subboard
type GameState = (Player, Maybe Coord, [[SubBoard]])
type Rating = Int

-- UPDATE GAME STATE (FIXED)
updateGameState :: GameState -> BigMove -> Maybe GameState
updateGameState (p, subBoard, boards) move@(outer, inner) =
  if valid
  then let nextBoard = changeBoard outer inner p boards
       in case nextBoard of
            Nothing -> Nothing
            Just board -> let sb = fromJust $ findSubBoard inner board 
                              -- board is the updated board
                              -- fromJust is safe bc coords are superficially valid
                              finished = (not $ isInProgress $ sb)
                              nextMove = if finished then Nothing else Just inner
                          in Just (opponent p, nextMove, board)
  else Nothing
  where valid = case subBoard of
                  Nothing -> checkBigMove move outer
                  Just sb -> checkBigMove move sb

opponent :: Player -> Player
opponent X = O
opponent O = X

changeBoard :: Coord -> Coord -> Player -> [[SubBoard]] -> Maybe [[SubBoard]]
changeBoard (outerX, 1) (innerX, innerY) val (b:bs) = unpackHead changed bs
  where changed = changeSubBoardRow outerX (innerX, innerY) val b
changeBoard (outerX, outerY) (innerX, innerY) val (b:bs) = unpackNext b next
  where next = changeBoard (outerX, outerY - 1) (innerX, innerY) val bs

changeSubBoardRow :: Int -> Coord -> Player -> [SubBoard] -> Maybe [SubBoard]
changeSubBoardRow 1 (x, y) val (b:bs) = unpackHead changed bs
  where changed = changeSubBoard (x, y) val b
changeSubBoardRow oX (x, y) val (b:bs) = unpackNext b next
  where next = changeSubBoardRow (oX - 1) (x, y) val bs

changeSubBoard :: Coord -> Player -> SubBoard -> Maybe SubBoard
changeSubBoard coord val (InProgress board) =
  case changed of
    Nothing -> Nothing -- error
    Just smth -> case checkWinner (InProgress smth) of
                   Nothing -> Just $ InProgress smth
                   Just winner -> Just $ Finished winner
  where changed = aux coord val board
        aux :: Coord -> Player -> [[Maybe Player]] -> Maybe [[Maybe Player]]
        aux (x, 1) val (b:bs) = unpackHead (changeRow x val b) bs
        aux (x, y) val (b:bs) = unpackNext b (aux (x, y - 1) val bs)
changeSubBoard (x, y) val (Finished winner) = Nothing -- error

changeRow :: Int -> Player -> [Maybe Player] -> Maybe [Maybe Player] -- this changes a row
changeRow 1 val (b:bs) =
  case b of
    Nothing -> Just $ (Just val):bs -- only time nothing is good (bc it's an empty space not an error)
    otherwise -> Nothing
changeRow index val (b:bs) = unpackNext b (changeRow (index - 1) val bs)

checkBigMove ((x1, y1), (x2, y2)) (x3, y3) = (x1, y1) == (x3, y3) && (and $ map (\x -> x > 0 && x <= 3) [x1, y1, x2, y2]) -- True if valid, False if not

unpackHead head next =
  case head of
    Nothing -> Nothing
    Just smth -> Just $ smth:next

unpackNext head next =
  case next of
    Nothing -> Nothing
    Just smth -> Just $ head:smth

-- FIND LEGAL MOVES AND HELPERS ETC.
findLegalMoves :: GameState -> [BigMove]

-- Finds sub board and returns all legal moves in it. If it is finished, calls function below
findLegalMoves (player, Just subCoords, board) = 
  case findSubBoard subCoords board of 
    Nothing -> error "Not a valid sub-board"
    Just subBoard@(InProgress _) -> allPairs subCoords (openSpaces subBoard) 
    Just (Finished w) -> findLegalMoves (player, Nothing, board)

-- When no next coord is specified, finds all in progress boards and returns all legal moves
findLegalMoves (player, Nothing, board) = 
  let labeledBoards = concat $ [labelRow x row | (x,row) <- zip [1..] board]
      labelRow x row  = [((y,x), sb) | (y,sb) <- zip [1..] row]
  in concat [allPairs (x,y) (openSpaces sb) | ((x,y), sb) <- labeledBoards]

-- Finds a sub board given a coord. Isolates row of boards given y coord, then picks the right one based on x coord. 
-- Will likely rewrite
findSubBoard :: Coord -> [[SubBoard]] -> Maybe SubBoard
findSubBoard (x, y) bigBoard = 
  do boardRow <- safeIndex y bigBoard
     safeIndex x boardRow

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
openSpaces (InProgress board) = concat [allPairs2 i (findNothingIdx row) | (i,row) <- zip [1..] board]
openSpaces _ = []

allPairs2 :: a -> [b] -> [(b,a)]
allPairs2 single lst = [(lstElem, single) | lstElem <- lst]

allPairs :: a -> [b] -> [(a,b)]
allPairs single lst = [(single, lstElem) | lstElem <- lst]

isInProgress :: SubBoard -> Bool
isInProgress (InProgress _) = True
isInProgress _ = False

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

isFull :: [[Maybe Player]] -> Bool
isFull (p) = not $ any (==Nothing) (concat p) -- if length[s |s <- p, length(catMaybes s)==3]==3 then Finished Tie else InProgress p

--[] = Finished Tie
--isFull (s:sb) = if length (catMaybes s) == 3 then isFull sb else sb

whoWillWin :: GameState -> Winner
whoWillWin gs@(p, sb, sbs) = 
  let nextGames = catMaybes [updateGameState gs m | m <- possibleMoves gs]
      eWinners = map whoWillWin nextGames
  in case winnerB gs of
      Just smth -> smth
      Nothing -> bestFor p eWinners

bestFor :: Player -> [Winner] -> Winner
bestFor p winners 
  | (Champ p) `elem` winners = (Champ p)  
  | Tie `elem` winners = Tie    
  | otherwise = Champ (opponent p)

possibleMoves :: GameState -> [BigMove]
possibleMoves (_, Just (x,y), _) = 
  [((x,y),(a,b)) | a <- [1..3], b <- [1..3]]

possibleMoves (_, Nothing, _) = 
  [((x,y), (a,b)) | x <- [1..3], y <- [1..3], a <- [1..3], b <- [1..3]]

whoMightWin :: GameState -> Int -> (Rating, Maybe BigMove)
whoMightWin gs 0 = (rateGame gs, Nothing)
whoMightWin gs@(p, _, _) depth = 
  case winnerB gs of
    Just outcome -> (rateOutcome outcome, Nothing)
    Nothing -> let 
               games =  findValidGameStates [(m, updateGameState gs m) | m <-  (possibleMoves gs)]
               ratings = [(fst $ whoMightWin game (depth-1), Just move) | (move, game) <- games]
               in --traceShow (gs, map fst games, winnerB gs) $
                 case p of
                   X -> maximum ratings
                   O -> minimum ratings



rateOutcome :: Winner -> Rating
rateOutcome (Champ X) =  3
rateOutcome (Champ O) = -3
rateOutcome Tie       =  0

bestMove :: GameState -> Maybe BigMove
bestMove gs = 
  case winnerB gs of
    Just smth -> Nothing
    Nothing -> Just $ bestMoveHelper gs

bestMoveHelper :: GameState -> BigMove 
bestMoveHelper gs@(player, move, board) =
  let movesAndResults = findValidGameStates $ map (\x -> (x, updateGameState gs x)) (possibleMoves gs)
      bestMoves = map (\(move, gs) -> (move, whoWillWin gs)) movesAndResults
  in fst $ case find (\(m, res) -> res == Champ player) bestMoves of 
      Just smth -> smth
      Nothing -> case find (\(m, res) -> res == Tie) bestMoves of
                  Just smth -> smth
                  Nothing -> head bestMoves --no move that can result in a win or tie, just move randomly

findValidGameStates :: [(BigMove, Maybe GameState)] -> [(BigMove, GameState)]
findValidGameStates lst = [(move, maybeState) | (move, Just maybeState) <- lst]

readGame :: String -> GameState
readGame text =
  let [player, next, board] = lines text
      bigRows = splitOn "|" board
      subBoards = map (splitOn ",") bigRows
      smallRows = map (map words) subBoards
      -- oneLine2 = map (map words . splitOn ",") (splitOn "|" board)

      charToPlayer c
        | c == 'X' = Just X
        | c == 'O' = Just O
        | c == '_' = Nothing
        | otherwise = error "Encounterd character other than 'X', 'O', or '_', in subboards, text representation likely corrupted."

      readSmallRow = map charToPlayer

      readSubBoard lst@[_,_,_] = InProgress (map readSmallRow lst)
      readSubBoard [p]
        | p == "T"  = Finished Tie
        | p == "X"  = Finished (Champ X)
        | p == "O"  = Finished (Champ O)
        | otherwise = error "Invalid singleton subboard (string other than \"X\", \"O\", or \"T\")"
      readSubBoard _ = error "Subboard invalid length"

      readPlayer p
        | p == "X" = X
        | p == "O" = O
        | otherwise = error "Invalid player's turn"
                    
      readNext "(0,0)" = Nothing
      readNext ['(', x, ',', y, ')'] = Just (digitToInt x, digitToInt y)
      readNext _ = error "Incorrectly formatted Coord in text representation"
  in (readPlayer player, readNext next, map (map readSubBoard) smallRows)

showGame :: GameState -> String
showGame (player, next, board) =
  let showNext (Just coord) = show coord
      showNext Nothing = "(0,0)"

      showBigRow r = init (concatMap showSubBoard r) ++ "|"
      showSubBoard (InProgress sb) = init (concatMap showSmallRow sb) ++ ","
      showSubBoard (Finished (Champ p)) = show p ++ ","
      showSmallRow r = concatMap showCell r ++ " "
      showCell (Just p) = show p
      showCell Nothing = "_"

  in show player ++ "\n" ++ showNext next ++ "\n" ++ init (concatMap showBigRow board)

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

-- gives a rating from -3 to 3 (3 is a win for X and -3 is a win for O)

rateGame :: GameState -> Rating
rateGame gs@(p, next, board) = if (abs min) > max then min else if (abs min) == max then 0 else max
  where b = flatten board
        ratingsLst = (ratingRow b) ++ (ratingCol b) ++ (ratingDia b)
        max = maximum ratingsLst
        min = minimum ratingsLst


flatten :: [[SubBoard]] -> [[Maybe Winner]]
flatten board = map flattenRow board
  where flattenRow :: [SubBoard] -> [Maybe Winner]
        flattenRow row = map flattenSB row 
          where flattenSB :: SubBoard -> Maybe Winner
                flattenSB sb = case sb of
                                 InProgress _ -> Nothing
                                 Finished x -> Just x

ratingRow :: [[Maybe Winner]] -> [Rating]
ratingRow board = map (\x -> count (catMaybes x)) board
        --if (abs mini) > maxi then mini else if (abs mini) == maxi then 0 else m
        --maxi = maximum lst
        --mini = minimum lst

ratingCol :: [[Maybe Winner]] -> [Rating]
ratingCol board = ratingRow (transpose board)

ratingDia :: [[Maybe Winner]] -> [Rating]
ratingDia board = [ratingBackDia board, ratingForDia board]
  where ratingBackDia board@[row1, row2, row3] = count (catMaybes [fst, snd, thrd])
          where fst = head row1
                snd = head (tail row2)
                thrd = last row3
        ratingForDia board = ratingBackDia (transpose board)

count :: [Winner] -> Rating
count lst =
  if rawTie == 0
  then if rawO == 0 then rawX else if rawX == 0 then -rawO else 0 -- 0 if obstructed
  else 0
  where rawX = length $ filter (==(Champ X)) lst
        rawO = length $ filter (==(Champ O)) lst
        rawTie = length $ filter (==(Tie)) lst

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
