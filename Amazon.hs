import Data.List
import Test.QuickCheck

data Board = Board {rows :: [[Tile]]}
    deriving (Show, Eq)

data Tile = Black | White | Arrow | Empty
    deriving (Show, Eq)

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

type Pos = (Int,Int)

-- For random generation
data APos = APos { p :: Pos}
    deriving (Show, Eq)

-- Generates random position
instance Arbitrary APos where
  arbitrary = do
    x <- choose (0,9)
    y <- choose (0,9)
    return (APos (x,y))

initialBoard :: Board
initialBoard = Board [(topRow Black),blankRow,blankRow,(middleRow Black),blankRow,blankRow,(middleRow White),blankRow,blankRow,(topRow White)]
    where
        blankRow :: [Tile]
        blankRow = replicate 10 Empty

        topRow :: Tile -> [Tile]
        topRow t = replace 6 t (replace 3 t blankRow)

        middleRow :: Tile -> [Tile]
        middleRow t = replace 0 t (replace 9 t blankRow )

--Replaces the given position in the board with the given tile.
replaceM :: Pos -> Tile -> Board -> Board
replaceM (x,y) t b | not (validPos (x,y)) = b
                   | otherwise = Board (replace y (replace x t row) (rows b))
        where 
            row = head (drop y (rows b))

prop_replaceM :: APos -> Tile -> Board -> Property
prop_replaceM ap t b = t /= getPos b pos ==> (getPos (replaceM pos t b) pos == t)
  where pos = (p ap)

replace :: Int -> a -> [a] -> [a]
replace _ _ []  = []
replace n a list | n < 0 || (n+1) > length list = list
                 | otherwise = take n list ++ [a] ++ drop (n + 1) list

{-
Moves the tile at the given position to the other given position.
Checks if positions are valid and there is a clear path between them.
Also checks if the piece is an amazon and not empty or arrow.
-}
move :: Board -> Pos -> Pos -> Board
move b p1 p2 | not (clearPath b p1 p2) = b
             | piece == Empty = b
             | piece == Arrow = b
             | otherwise = replaceM p2 piece (replaceM p1 Empty b)
        where 
            piece = getPos b p1

-- Gives up, needs better condition
prop_move :: Board -> Tile -> APos -> Property
prop_move b t pos = b /= newBoard ==> (getPos newBoard p1 == Empty) && (getPos newBoard p2 == getPos b p1)
  where p2 = (p pos)
        p1 = head (findTiles b t)
        newBoard = move b p1 p2

--Shoots an arrow to the given space. Checks if the shot is possible but not if there is an amazon available to shoot it.
shoot :: Board -> Pos -> Pos -> Board
shoot b p1 p2 | not (clearPath b p1 p2) = b
              | otherwise = replaceM p2 Arrow b

prop_shoot :: Board -> APos -> APos -> Property
prop_shoot b pos1 pos2 = b /= newBoard ==> getPos newBoard p2 == Arrow
  where p1 = (p pos1)
        p2 = (p pos2)
        newBoard = shoot b p1 p2

--TODO:props
--Not moving at all -> True or false?
clearPath :: Board -> Pos -> Pos -> Bool
clearPath b (x1,y1) (x2,y2) | not (validPos (x1,y1) && validPos (x2,y2)) = False
                            | not straightLine = False
                            | otherwise = emptyPath b (x1,y1) (x2,y2)
    where
        --If we aren't moving straight up or down, we have to be moving the same amount of tiles in both x and y direction.
        straightLine = ((x1 == x2 ) || (y1 == y2) ) || (abs (x1 - x2) == abs (y1-y2))
        --Checks if the given path is empty or not
        emptyPath :: Board -> Pos -> Pos -> Bool
        emptyPath b (x1,y1) (x2,y2) | y1 == y2 = all (==Empty) (take (x2-x1) (drop x1 row))
                                    | x1 == x2 = all (==Empty) (take (y2-y1) (drop y1 row))
                                    | north && east = diagonalPath b (x1,y1) (x2,y2) 1 1
                                    | east = diagonalPath b (x1,y1) (x2,y2) 1 (-1)
                                    | north = diagonalPath b (x1,y1) (x2,y2) (-1) 1
                                    | otherwise = diagonalPath b (x1,y1) (x2,y2) (-1) (-1)

            where row = head (drop y1 (rows b))
                  row2 = head (drop x1 (transpose (rows b)))
                  east = x2 > x1
                  north = y2 > y1
        --Checks if the diagonal path given is empty or not.
        diagonalPath :: Board -> Pos -> Pos -> Int -> Int -> Bool
        diagonalPath b (x1,y1) (x2,y2) dx dy | x1 == x2 = True
                                             | (getPos b (x1,y1)) /= Empty = False
                                             | otherwise = diagonalPath b ((x1+dx),(y1+dy)) (x2,y2) dx dy

--Checks if the given position is valid (inside the board)
validPos :: Pos -> Bool
validPos (x,y) = (x >= 0 && x <=9) && (y >= 0 && y <=9)

--Checks if the game is over and returns the color of the victor, empty if not over.
--TODO: Fix ugly.
gameOver :: Board -> Tile
gameOver b = undefined
    where
        overFor :: Board -> Tile -> Bool
        overFor b t = undefined
            --findTiles b t

        tilesAround :: Board -> [Pos] -> [Tile]
        tilesAround = undefined

--Returns the tile at the given position after checking that it is valid.
getPos :: Board -> Pos -> Tile
getPos b (x,y) | validPos (x,y) = ((rows b)!!y)!!x
               | otherwise = undefined

findTiles :: Board -> Tile -> [Pos]
findTiles b t = findInRow (rows b) t 0
    where
        findInRow :: [[Tile]] -> Tile -> Int -> [Pos]
        findInRow [] _ _ = []
        findInRow _ _ 10 = []
        findInRow (x:xs) t y = (zip (elemIndices t x) yList) ++ findInRow xs t (y+1)
            where
                yList = replicate 10 y

printBoard :: Board -> IO ()
printBoard b = putStrLn (oneString (rows b))
  where
    oneString :: [[Tile]] -> String
    oneString r = unlines (map lineToString r)
     where
      lineToString :: [Tile] -> String
      lineToString l = map tileToChar l
        where
          tileToChar :: Tile -> Char
          tileToChar Black = 'B'
          tileToChar White = 'W'
          tileToChar Arrow = 'X'
          tileToChar Empty = '.'
