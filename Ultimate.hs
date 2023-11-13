module Ultimate where

data Player = X | O deriving (Show, Eq)
data Winner = Champ Player | Tie deriving (Show, Eq)
data SubBoard = InProgress [[Maybe Player]] | Finished Winner deriving (Show, Eq)
type Coord = (Int, Int)
type BigMove = (Coord, Coord) -- first coord is the location of the SUBBOARD second is for the spot on the subboard
type GameState = (Player, Maybe Coord, [[SubBoard]])
-- GameState should be a type, not a variable type def

-- functions for
  -- winner of a game state (GameState -> Winner) Joseph
  -- update game state (GameState -> Move -> GameState) Gaya
  -- legal moves (Game -> [Move]) Jorge
  -- pretty show function (Game -> String) Blake
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