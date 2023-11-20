module Ultimate where
import Data.Maybe 

data Player = X | O deriving (Show, Eq)
data Winner = Champ Player | Tie deriving (Show, Eq)
data SubBoard = InProgress [[Maybe Player]] | Finished Winner deriving (Show, Eq)
type Coord = (Int, Int)
type BigMove = (Coord, Coord) -- first coord is the location of the SUBBOARD second is for the spot on the subboard
type GameState = (Player, Maybe Coord, [[SubBoard]])

sb = InProgress [[Just X, Just O, Just X], [Just O, Just X, Nothing], [Nothing, Nothing, Just O]]
row = [sb, sb, sb]
board = [row, amendedRow, row]
amendedRow = [sb, sb, Finished $ Champ X]

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
