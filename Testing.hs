module Testing where

import Data.Maybe
import Logic
import Test.QuickCheck

-- Generates a random tile
instance Arbitrary Tile where
  arbitrary = oneof [return Black,
                    return White,
                    return Arrow,
                    return Empty]

-- Generates a weighted tile for arbitrary boards
tile :: Gen(Tile)
tile = frequency [(10, return Empty),
                  (2, oneof [return Black,
                             return White,
                             return Arrow])]

-- Generates a randomized board
instance Arbitrary Board where
  arbitrary =
    do rows <- sequence [ sequence [ tile | j <- [1..10] ] | i <- [1..10] ]
       return (Board rows)

board :: Gen(Board)
board = do rows <- sequence [ sequence [ tile | j <- [1..10] ] | i <- [1..10] ]
           return (Board rows)

-- For random generation
data APos = APos { p :: Pos}
    deriving (Show, Eq)

-- Generates random position
instance Arbitrary APos where
  arbitrary = do
    x <- choose (0,9)
    y <- choose (0,9)
    return (APos (x,y))

-- A board where only white has any moves left
gameOverBoardWhite :: Board
gameOverBoardWhite = Board (lastRow:(replicate 9 filledRow))
    where
      lastRow :: [Tile]
      lastRow = [White, Empty] ++ (replicate 7 Arrow) ++ [Black]

-- A board where only black has any moves left
gameOverBoardBlack :: Board
gameOverBoardBlack = Board (lastRow:(replicate 9 filledRow))
    where
      lastRow :: [Tile]
      lastRow = [White] ++ (replicate 7 Arrow) ++ [Black, Empty]

filledRow :: [Tile]
filledRow = replicate 10 Arrow

-- A board for testing movement in all directions
movementBoard = Board( 
                [[Black,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Black,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Empty,White,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,White]])

-- A board for testing that illegal moves are not made
illegalMoveBoard = Board( 
                [[Black,Arrow,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                 [Arrow,Arrow,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Arrow,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Black,Black],
                 [Black,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Black,White]])

-- Checks that a board is a valid size, i.e 10x10
prop_isValidBoard :: Board -> Bool
prop_isValidBoard b = (length r == 10) && all (== 10) (map length r)
    where r = (rows b)

-- Checks that the correct amount of amazons are on the board
prop_correctAmazons :: Board -> Bool
prop_correctAmazons b = (length (findTiles b Black) == 4) && 
                        (length (findTiles b White) == 4)

-- Checks that the game is in a valid state
prop_validGameState :: Board -> Bool
prop_validGameState b = prop_correctAmazons b &&
                        prop_isValidBoard b

-- Checks that gameOver function is correct
prop_gameOver :: Bool
prop_gameOver = (gameOver gameOverBoardBlack == Black) &&
                (gameOver gameOverBoardWhite == White) &&
                (gameOver initialBoard == Empty)

-- Checks that a shoot actually yields an arrow in the targeted tile
prop_shoot :: Board -> APos -> APos -> Property
prop_shoot b pos1 pos2 = b /= newBoard ==> getPos newBoard p2 == Just Arrow
  where p1 = (p pos1)
        p2 = (p pos2)
        newBoard = shoot b p1 p2

-- Checks that a tile actually is replaced
prop_replacePos :: APos -> Tile -> Board -> Property
prop_replacePos ap t b = Just t /= getPos b pos ==> (getPos (replacePos pos t b) pos == Just t)
  where pos = (p ap)

-- Tests moves in all allowed directions.
prop_allMoves :: Bool
prop_allMoves = (getPos forward (8,0) == Just Black) &&
                (getPos down (0,8) == Just Black) &&
                (getPos up (9,0) == Just White) &&
                (getPos backward (0,9) == Just White) &&
                (getPos northeast (7,1) == Just Black) &&
                (getPos northwest (1,1) == Just Black) &&
                (getPos southeast (8,8) == Just White) &&
                (getPos southwest (2,8) == Just White)
  where
    -- The different moves to be tested, named after direction
    forward = move movementBoard (0,0) (8,0)
    down = move movementBoard (0,0) (0,8)
    up = move movementBoard (9,9) (9,0)
    backward = move movementBoard (9,9) (0,9)
    northeast = move movementBoard (4,4) (7,1)
    northwest = move movementBoard (4,4) (1,1)
    southeast = move movementBoard (5,5) (8,8)
    southwest = move movementBoard (5,5) (2,8)

{-
  Checks that illegal moves are not made.

  As the function move returns the original board when
  an illegal move is attempted, this is done by making
  some illegal move on a board and checking that they 
  have not changed.
-}
prop_illegalMoves :: Bool
prop_illegalMoves = all (== illegalMoveBoard) listofBoards
  where
    blackOverArrow = move illegalMoveBoard (0,0) (0,9)
    whiteOverAmazon = move illegalMoveBoard (9,9) (9,0)
    moveArrow = move illegalMoveBoard (4,4) (4,1)
    moveEmpty = move illegalMoveBoard (5,5) (5,8)
    blackNonStraight = move illegalMoveBoard (0,9) (5,3)
    listofBoards = [blackOverArrow] ++ [whiteOverAmazon] 
                    ++ [moveArrow] ++ [moveEmpty] ++ [blackNonStraight]