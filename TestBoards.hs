import Ultimate

-- emptySubBoard :: SubBoard
-- emptySubBoard   = replicate 3 (replicate 3 Nothing)
-- allXs = replicate 3 (replicate 3 (Just X))
-- allOs = replicate 3 (replicate 3 (Just O))
-- tiedSubBoard = replicate 3 (replicate 3 "-")

{-
[X] [ ] [O] | [O] [ ] [ ] | [X] [O] [ ]
[O] [X] [X] | [X] [X] [ ] | [ ] [ ] [ ]
[ ] [ ] [O] | [ ] [ ] [ ] | [O] [O] [ ]
------------|-------------|------------
[ ] [X] [X] | [ ] [O] [ ] | [O] [ ] [ ]
[ ] [O] [ ] | [ ] [ ] [ ] | [ ] [O] [ ]
[ ] [ ] [ ] | [X] [X] [X] | [ ] [ ] [O]
------------|-------------|------------
[ ] [ ] [X] | [ ] [ ] [ ] | [X] [ ] [ ]
[ ] [ ] [X] | [ ] [ ] [ ] | [ ] [ ] [ ]
[ ] [ ] [X] | [O] [ ] [X] | [O] [O] [O]
-}


testSubBoard1_1, testSubBoard2_1, testSubBoard3_1, testSubBoard1_2, testSubBoard2_3 :: SubBoard
testSubBoard1_1 = InProgress [[Just X, Nothing, Just O], [Just O, Just X, Just X], [Nothing, Nothing, Just O]]
testSubBoard2_1 = InProgress [[Just O, Nothing, Nothing], [Just X, Just X, Nothing], [Nothing, Nothing, Nothing]]
testSubBoard3_1 = InProgress [[Just X, Just O, Nothing], [Nothing, Nothing, Nothing], [Just O, Just O, Nothing]]
testSubBoard1_2 = InProgress [[Nothing, Just X, Just X], [Nothing, Just O, Nothing], [Nothing, Nothing, Nothing]]
testSubBoard2_3 = InProgress [[Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing], [Just O, Nothing, Just X]]


testBoard :: GameState
testBoard = (X, Just (2, 3), [[testSubBoard1_1, testSubBoard2_1, testSubBoard3_1],
                              [testSubBoard1_2, Finished (Champ X), Finished (Champ O)],
                              [Finished (Champ X), testSubBoard2_3, Finished (Champ O)]])

almostFinished :: SubBoard
almostFinished = InProgress [[Nothing, Just O, Just X], [Just X, Nothing, Just O], [Just O, Just X, Just X]]
-- almostFinished2 = InProgress [[Just X, Nothing, Just O],[Nothing, Just O, Just X], [Just O, Nothing, Nothing]]
-- almostFinished = InProgress [[Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing]]

nearlyDoneTest :: GameState
nearlyDoneTest = (O, Nothing, [
    [Finished (Champ X), Finished (Champ O), almostFinished],
    [almostFinished,     Finished (Champ X), Finished (Champ X)],
    [almostFinished, Finished (Champ O), almostFinished]])


emptyRow :: [Maybe Player]
emptyRow = [Nothing, Nothing, Nothing]
emptySubBoard = InProgress $ replicate 3 emptyRow 
emptyGameBoardRow = replicate 3 emptySubBoard
emptyGameBoard = replicate 3 emptyGameBoardRow

emptyGameState = (O, Nothing, emptyGameBoard)

tieGameBoard :: [[SubBoard]]
tieGameBoard = [[Finished (Champ O), Finished (Champ O), Finished (Champ X)], 
                [Finished (Champ X), Finished (Champ X), Finished (Champ O)],
                [Finished (Champ O), Finished (Champ X), Finished (Champ X)]]

tieGameState = (X, Nothing, tieGameBoard)

classGame1_1 = InProgress [[Just O, Just X, Just X], [Just X, Nothing, Just O], [Nothing, Nothing, Just X]]
classGame2_1 = InProgress [[Just O, Just X, Nothing], [Just X, Just O, Nothing], [Just X, Nothing, Nothing]]
classGame3_2 = InProgress [[Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing], [Just O, Just X, Just X]]
testUpdateGameStateBoard :: [[SubBoard]]
testUpdateGameStateBoard = [[classGame1_1, classGame2_1, Finished (Champ O)],
                            [Finished (Champ O), Finished (Champ X), Finished (Champ X)],
                            [Finished (Champ O), Finished (Champ X), Finished (Champ O)]]

testUpdateGameState :: GameState
testUpdateGameState = (X, Just (1,1), testUpdateGameStateBoard)

textRep :: String
textRep = "Player\nCoord\nX_OOXX__O,O__XX____,XO____OO_|_XX_O____,X,O|X,______O_X,O"
