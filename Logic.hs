module Logic where

import Data.List
import Data.Maybe

data Board = Board {rows :: [[Tile]]}
    deriving Eq

data Tile = Black | White | Arrow | Empty
    deriving (Show, Eq)

instance Show Board where
  show (Board rows) = boardToString rows

type Pos = (Int,Int)

-- Produces the initial board.
initialBoard :: Board
initialBoard = Board [(topRow Black),
                      blankRow,
                      blankRow,
                      (middleRow Black),
                      blankRow,
                      blankRow,
                      (middleRow White),
                      blankRow,
                      blankRow,
                      (topRow White)]
    where
        blankRow :: [Tile]
        blankRow = replicate 10 Empty

        topRow :: Tile -> [Tile]
        topRow t = replace 6 t (replace 3 t blankRow)

        middleRow :: Tile -> [Tile]
        middleRow t = replace 0 t (replace 9 t blankRow )

-- Replaces the given position in the board with the given tile.
replacePos :: Pos -> Tile -> Board -> Board
replacePos (x,y) t b | not (validPos (x,y)) = b
                     | otherwise = Board (replace y (replace x t row) (rows b))
        where 
            row = head (drop y (rows b))

-- Replaces the element at the given index with the given element.
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
             | otherwise = replacePos
             p2 piece (replacePos
             p1 Empty b)
        where 
            piece = fromJust (getPos b p1)

{-
  Shoots an arrow to the given space. Checks if the shot is possible 
  but not if there is an amazon available to shoot it.
-}
shoot :: Board -> Pos -> Pos -> Board
shoot b p1 p2 | not (clearPath b p1 p2) = b
              | otherwise = replacePos
             p2 Arrow b

{-
  Checks if the path between two positions is a straight or diagonal line.
  If it is, checks if the tiles between them, excluding the first and
  including the last, are all empty
-}
clearPath :: Board -> Pos -> Pos -> Bool
clearPath b (x1,y1) (x2,y2) | not (validPos (x1,y1) && validPos (x2,y2)) = False
                            | x1 == x2 && y1 == y2 = False
                            | not straightLine = False
                            | x1 == x2 = checkPath b (x1,y1) (x2,y2) 0 deltaY
                            | y1 == y2 = checkPath b (x1,y1) (x2,y2) deltaX 0
                            | otherwise = checkPath b (x1,y1) (x2,y2) deltaX deltaY
    where 
        deltaX = div (x2 - x1) (abs (x2-x1))
        deltaY = div (y2 - y1) (abs (y2-y1))
        {-
          If we aren't moving straight up or down, we have to be 
          moving the same amount of tiles in both x and y direction.
        -}
        straightLine = ((x1 == x2 ) || (y1 == y2) ) || (abs (x1 - x2) == abs (y1-y2))

        -- Checks if the path given is empty or not.
        checkPath :: Board -> Pos -> Pos -> Int -> Int -> Bool
        checkPath b (x1,y1) (x2,y2) dx dy | (x1 == x2) && (y1 == y2) = True
                                          | fromJust (getPos b ((x1+dx),(y1+dy))) /= Empty = False
                                          | otherwise = checkPath b ((x1+dx),(y1+dy)) (x2,y2) dx dy

-- Checks if the given position is valid (inside the board)
validPos :: Pos -> Bool
validPos (x,y) = (x >= 0 && x <=9) && (y >= 0 && y <=9)

-- Checks if the game is over and returns the color of the victor, empty if not over.
gameOver :: Board -> Tile
gameOver b | overFor b White = Black
           | overFor b Black = White
           | otherwise = Empty
    where
        overFor :: Board -> Tile -> Bool
        overFor b t = all (/=Empty) (concat (map (\x -> tilesAround b x) (findTiles b t)))

-- Given a position and a board, returns all tiles around that tile.
tilesAround :: Board -> Pos -> [Tile]
tilesAround b (x,y) = map fromJust ( [nw,ne,se,sw,n,s,e,w] \\ [Nothing])

            where 
                nw = getPos b ((x-1),(y-1))
                ne = getPos b ((x+1),(y-1))
                se = getPos b ((x+1),(y+1))
                sw = getPos b ((x-1),(y+1))
                n = getPos b (x,(y-1))
                s = getPos b (x,(y+1))
                e = getPos b ((x+1),y)
                w = getPos b ((x-1),y)


-- Returns the tile at the given position after checking that it is valid.
getPos :: Board -> Pos -> Maybe Tile
getPos b (x,y) | validPos (x,y) = Just (((rows b)!!y)!!x)
               | otherwise = Nothing

-- Given a board and a tile returns all positions where that tile can be found.
findTiles :: Board -> Tile -> [Pos]
findTiles b t = findInRow (rows b) t 0
    where
        findInRow :: [[Tile]] -> Tile -> Int -> [Pos]
        findInRow [] _ _ = []
        findInRow _ _ 10 = []
        findInRow (x:xs) t y = (zip (elemIndices t x) yList) ++ findInRow xs t (y+1)
            where
                yList = replicate 10 y

-- Prints a given board
printBoard :: Board -> IO ()
printBoard b = putStrLn (boardToString (rows b))

-- Takes a board and creates a printable version with coordinates.
boardToString :: [[Tile]] -> String
boardToString t = unlines (addCoordinates (map lineToString (t)))
     where
      lineToString :: [Tile] -> String
      lineToString l = map tileToChar l
        where
          tileToChar :: Tile -> Char
          tileToChar Black = 'B'
          tileToChar White = 'W'
          tileToChar Arrow = 'X'
          tileToChar Empty = '.'

{-
  Adds coordinates around a board making it easier to see 
  where to move from and where to move to
-}
addCoordinates :: [String] -> [String]
addCoordinates list = ("  "++coordinates)
                      :transpose((" "++coordinates)
                      :transpose((" "++whitespace)
                      :transpose(whitespace
                      :(transpose list))))
  where coordinates = "0123456789"
        whitespace = "          "

