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
isInProgress :: SubBoard -> Bool
isInProgress (InProgress _) = True
isInProgress _ = False

winnerB :: GameState -> Maybe Winner
winnerB (p, mc, sbs) = undefined
  
  --let
  --winLine = [l |l <-lineLst, (checkLine l) /= Nothing]
  --in if winLine == [] then Nothing else Just (checkLine (head winLine)) 

--missing case for full board tie game. Functionality not confirmed.XX

-- checkLine :: Line -> SubBoard -> Maybe Winner
-- checkLine (ci:ca:co) sb  = undefined
-- if ci == Finished x && ci == ca == co then Just x else Nothing
--fix Logic?XXXXX

miniWinner :: SubBoard -> Maybe Winner
miniWinner sb = let
  cs = (checkWinner sb)
  in if not (null cs) then cs
  else if isFull sb then Just Tie else Nothing


checkWinner :: SubBoard -> Maybe Winner
checkWinner sb = let
  cd = checkDia sb
  cr = checkRow sb
  cc = checkCol sb
            in if  cd /= Nothing then Just (Champ (fromJust cd))
            else if cr /= Nothing then Just (Champ (fromJust cr))
            else if cc /= Nothing then Just (Champ (fromJust cc))
            else Nothing

checkDia :: SubBoard -> Maybe Player
checkDia (InProgress sb) = let
  tl = head (head sb)
  tr = last (head sb)
  mid = head (tail (head (tail sb)))
  bl = head (last sb)
  br = last (last sb)
  in if ((tl == mid && mid == br) || (tr == mid && mid == bl)) then mid else Nothing
--(InProgress((i:x:t):(a:xs:ts):(o:xxs:tts))) = if (xs /= Nothing && ((i == xs && (head tts) == (head xs) ) || (o == xs && (head t)
-- = xs))) then xs else Nothing

checkRow :: SubBoard -> Maybe Player
checkRow (InProgress b) = firstJust $ map aux b -- if any (aux X) board then Just X else if any (aux O) board then Just O else Nothing
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

checkCol :: SubBoard -> Maybe Player
checkCol (InProgress b) = checkRow (InProgress(transpose b))
-- checkCol [] = Nothing
-- checkCol (InProgress((i:x):(a:xs):(o:xxs))) = 
--   if i /= Nothing && a == i && o == i then i else checkCol (x:xs:xxs)


--assignCoordinates :: SubBoard -> [(Coord, Maybe Player)]
--assignCoordinates (InProgress lst) = [((x, y), play) | (row, x) <- zip lst [1..], (play, y) <- zip row [1..]]

--  | map
--  | isFull sb = Tie
--  | otherwise = sb
-- else sb
--miniWinner sb = let
  --winLine = [l |l <-lineLst, (checkLine l) /= Nothing]
  --if winLine  =
isFull :: SubBoard -> Bool
isFull (InProgress p) = not $ any (==Nothing) (concat p) -- if length[s |s <- p, length(catMaybes s)==3]==3 then Finished Tie else InProgress p

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
