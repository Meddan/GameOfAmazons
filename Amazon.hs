import Data.List
data Board = Board {rows :: [[Tile]]}
    deriving (Show, Eq)

data Tile = Black | White | Arrow | Empty
    deriving (Show, Eq)

type Pos = (Int,Int)

initialBoard :: Board
initialBoard = Board [(topRow Black),blankRow,blankRow,(middleRow Black),blankRow,blankRow,(middleRow White),blankRow,blankRow,(topRow White)]
    where
        blankRow :: [Tile]
        blankRow = replicate 10 Empty

        topRow :: Tile -> [Tile]
        topRow t = replace 6 t (replace 3 t blankRow)

        middleRow :: Tile -> [Tile]
        middleRow t = replace 0 t (replace 9 t blankRow )

replace :: Int -> a -> [a] -> [a]
replace _ _ []  = []
replace n a list | n < 0 || (n+1) > length list = list
                 | otherwise = take n list ++ [a] ++ drop (n + 1) list

move :: Board -> Pos -> Pos -> Board
move b p1 p2 | not (clearPath b p1 p2) = error ""

--TODO:props
--Not moving at all -> True or false?
clearPath :: Board -> Pos -> Pos -> Bool
clearPath b (x1,y1) (x2,y2) | not (validPos (x1,y1) && validPos (x2,y2)) = False
                  | not straightLine = False
                  | otherwise = emptyPath b (x1,y1) (x2,y2)
    where 
        straightLine = ((x1 == x2 ) || (y1 == y2) ) || (abs (x1 - x2) == abs (y1-y2))

        emptyPath :: Board -> Pos -> Pos -> Bool
        emptyPath b (x1,y1) (x2,y2) | y1 == y2 = all (==Empty) (take (x2-x1) (drop x1 row))
                                    | x1 == x2 = all (==Empty) (take (y2-y1) (drop y1 row))
                                    | north && east = diagonalPath b (x1,y1) (x2,y2) 1 1
                                    | east = diagonalPath b (x1,y1) (x2,y2) 1 0
                                    | north = diagonalPath b (x1,y1) (x2,y2) 0 1
                                    | otherwise = diagonalPath b (x1,y1) (x2,y2) 0 0

            where row = head (drop y1 (rows b))
                  row2 = head (drop x1 (transpose (rows b)))
                  east = x2 > x1
                  north = y2 > y1

        diagonalPath :: Board -> Pos -> Pos -> Int -> Int -> Bool
        diagonalPath b (x1,y1) (x2,y2) dx dy | x1 == x2 = True
                                             | (getPos b (x1,y1)) /= Empty = False
                                             | otherwise = diagonalPath b ((x1+dx),(y1+dy)) (x2,y2) dx dy

validPos :: Pos -> Bool
validPos (x,y) = (x >= 0 && x <=9) && (y >= 0 && y <=9)

getPos :: Board -> Pos -> Tile
getPos b (x,y) = ((rows b)!!y)!!x