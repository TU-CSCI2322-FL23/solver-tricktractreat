module Ultimate where
import Data.Maybe
import Data.List

data Player = X | O deriving (Show, Eq)
data Winner = Champ Player | Tie deriving (Show, Eq)
data SubBoard = InProgress [[Maybe Player]] | Finished Winner deriving (Show, Eq)
type Coord = (Int, Int)
type BigMove = (Coord, Coord) -- first coord is the location of the SUBBOARD second is for the spot on the subboard
type GameState = (Player, Maybe Coord, [[SubBoard]])

--type Line = [Coord] --added line typeXX

--list of list of cordinates that form winning linesXX
--lineLst = [[(1,1),(2,2),(3,3)],[(3,1),(2,2),(1,3)],[(1,1),(1,2),(1,3)],
--           [(2,1),(2,2),(2,3)],[(3,1),(3,2),(3,3)],[(1,1),(2,1),(3,1)],[(1,2),(2,2),(3,2)],[(1,3),(2,3),(3,3)]]

-- GameState should be a type, not a variable type def

-- functions for
  -- winner of a game state (GameState -> Winner) Joseph
  -- update game state (GameState -> Move -> GameState) Gaya
  -- legal moves (Game -> [Move]) Jorge
  -- pretty show fuction (Game -> String) Blake
--  testSubBoard1_1, testSubBoard2_1, testSubBoard3_1, testSubBoard1_2, testSubBoard2_3 :: SubBoard
--  testSubBoard1_1 = InProgress [[Just X, Nothing, Just O], [Just O, Just X, Just X], [Nothing, Nothing, Just O]]
--  testSubBoard2_1 = InProgress [[Just O, Nothing, Nothing], [Just X, Just X, Nothing], [Nothing, Nothing, Nothing]]
--  testSubBoard3_1 = InProgress [[Just X, Just O, Nothing], [Nothing, Nothing, Nothing], [Just O, Just O, Nothing]]
--  testSubBoard1_2 = InProgress [[Nothing, Just X, Just X], [Nothing, Just O, Nothing], [Nothing, Nothing, Nothing]]
--  testSubBoard2_3 = InProgress [[Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing], [Just O, Nothing, Just X]]
 
--  testBoard :: GameState
--  testBoard = (X, Just (2, 3), [[testSubBoard1_1, testSubBoard2_1, testSubBoard3_1],
--                                [testSubBoard1_2, Finished (Champ X), Finished (Champ O)],
--                                [Finished (Champ X), testSubBoard2_3, Finished (Champ O)]])

isInProgress :: SubBoard -> Bool
isInProgress (InProgress _) = True
isInProgress _ = False

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
