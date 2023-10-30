data Player = X | O
data Winner = Champ Player | Tie 
data SubBoard = InProgress [[Maybe Player]] | Finished Winner
gameState :: (Player, Maybe SubBoard, [SubBoard])

functions for
-- is it complete
-- making a move
-- determining what the next board is 