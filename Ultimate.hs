data Player = X | O
data Winner = Champ Player | Tie 
data SubBoard = InProgress [[Maybe Player]] | Finished Winner
type gameState = (Player, Maybe SubBoard, [SubBoard])
-- gameState should be a type, not a variable type def

-- functions for
  -- is it complete
  -- making a move
  -- determining what the next board is 
