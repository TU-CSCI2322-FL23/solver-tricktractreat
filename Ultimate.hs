module Ultimate where
import Data.Maybe
import Data.List

data Player = X | O deriving (Show, Eq)
data Winner = Champ Player | Tie deriving (Show, Eq)
data SubBoard = InProgress [[Maybe Player]] | Finished Winner deriving (Show, Eq)
type Coord = (Int, Int)
type BigMove = (Coord, Coord) -- first coord is the location of the SUBBOARD second is for the spot on the subboard
type GameState = (Player, Maybe Coord, [[SubBoard]])

-- UPDATE GAME STATE (FIXED)
updateGameState :: GameState -> BigMove -> Maybe GameState
updateGameState (p, subBoard, boards) move@(outer, inner) =
  if valid
  then let finished = not $ isInProgress $ fromJust $ findSubBoard inner boards -- fromJust is safe bc coords are superficially valid
           nextMove = if finished then Nothing else Just inner
           nextBoard = changeBoard outer inner p boards
       in case nextBoard of
         Nothing -> Nothing
         Just board -> Just (opponent p, nextMove, board)
  else Nothing
  where valid = case subBoard of
                  Nothing -> checkBigMove move outer
                  Just sb -> checkBigMove move sb

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
changeSubBoard coord val (InProgress boards) =
  case changed of
    Nothing -> Nothing -- error
    Just smth -> Just $ InProgress smth
  where changed = aux coord val boards
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
      labelRow x row  = [((x,y), sb) | (y,sb) <- zip [1..] row]
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
openSpaces (InProgress board) = concat [allPairs i (findNothingIdx row) | (i,row) <- zip [1..] board]
openSpaces _ = []

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
possibleMoves :: GameState -> [BigMove]
possibleMoves (_, Just (x,y), _) = 
  [((x,y),(a,b)) | a <- [1..3], b <- [1..3]]

possibleMoves (_, Nothing, _) = 
  [((x,y), (a,b)) | x <- [1..3], y <- [1..3], a <- [1..3], b <- [1..3]]

whoMightWin :: Int -> GameState -> Maybe Winner
whoMightWin 0 = winnerB gs
whoMightWin 1 = let
  moves = possibleMoves gs
  pw = map winnerB $ catMaybes[updateGameState gs m | m <- moves]
  in if pw == [] then Nothing else if Just (Champ p) `elem` pw then Just (Champ p) else Just Tie 
whoMightWin depth gs@(p, sb, sbs) = let
      wgs = winnerB gs
      moves = possibleMoves gs
      pw = map winnerB $ (catMaybes[updateGameState gs m | m <- moves])
      in if not $ null wgs then wgs 
          else case ((catMaybes pw), p) of
               ([], X) -> maybeIntToWin (maximum(map maybeWinnerMinMax $ [whoMightWin (depth-1) (fromJust (updateGameState gs m)) | m <- moves]))
               ([], O) -> maybeIntToWin (minimum(map maybeWinnerMinMax $ [whoMightWin (depth-1) (fromJust (updateGameState gs m)) | m <- moves]))
               (arb, _) -> Just (head arb)

whoWillWin :: GameState -> Winner
whoWillWin gs@(p, sb, sbs) = let
  wgs = winnerB gs
  moves = possibleMoves gs
  pw = map winnerB $ catMaybes [updateGameState gs m | m <- moves]
  in case wgs of
     Just smth -> smth
     Nothing -> case ((catMaybes pw), p) of
                ([], X) -> intToWin (maximum(map playerMinMax $ map whoWillWin (catMaybes [(updateGameState gs m) | m <- moves])))
                ([], O) -> intToWin (minimum(map playerMinMax $ map whoWillWin (catMaybes [(updateGameState gs m) | m <- moves])))
                (arb, _) -> head arb

playerMinMax :: Winner -> Int
playerMinMax p = case p of
  Champ X -> 1
  Champ O -> -1
  Tie -> 0

maybeWinnerMinMax :: Maybe Winner -> Maybe Int
maybeWinnerMinMax w = case w of
  Nothing -> Nothing
  Just x -> Just (playerMinMax x)

maybeIntToWin :: Maybe Int -> Maybe Winner
maybeIntToWin i = case i of 
  Nothing -> Nothing
  Just x -> Just (intToWin x)

intToWin :: Int -> Winner
intToWin i = case i of
  -1 -> Champ O
  1 -> Champ X
  _ -> Tie
  


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
