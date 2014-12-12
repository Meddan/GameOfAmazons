import Data.List
import Test.QuickCheck
import System.Console.ANSI hiding (Black, White)
import System.IO
import Control.Monad
import Data.List.Split
import Data.Char
import Control.Concurrent

data Board = Board {rows :: [[Tile]]}
    deriving Eq

data Tile = Black | White | Arrow | Empty
    deriving (Show, Eq)

instance Show Board where
  show (Board rows) = boardToString rows

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

main :: IO ()
main = do 
  setTitle "Game of Amazons"
  putStrLn "Press i for instructions or anything else for the game"

  char <- getChar
  if char == 'i'
    then resetScreen >> instructions
    else resetScreen >> gameLoop White initialBoard

gameLoop :: Tile -> Board -> IO ()
gameLoop t b = do 
  resetScreen
  printBoard b
  putStrLn ("It is " ++ show(t) ++ "'s turn")
  line <- getLine
  putStrLn line
  let list = splitOn " " line
  if checkInput list
    then do 
      let p = head(parseInput list)
      let m = head(drop 1 (parseInput list))
      let a = head(drop 2 (parseInput list))
      if validateMove b t p m
        then do
          let newBoard = (move b p m)
          if validateMove newBoard t m a
            then do
              let finalBoard = shoot newBoard m a
              gameLoop t finalBoard
            else do 
                 (putStrLn "Illegal shot!")
                 threadDelay 1000000
                 gameLoop t b
        else do (putStrLn "Illegal move!")
                threadDelay 1000000
                gameLoop t b

    else do 
            (putStrLn "Incorrect formatting!" )
            threadDelay 1000000
            gameLoop t b

validateMove :: Board -> Tile -> Pos -> Pos -> Bool
validateMove b t p m | getPos b p /= t = False
                     | otherwise = clearPath b p m

resetScreen :: IO ()
resetScreen = clearScreen >> setSGR [Reset] >> setCursorPosition 0 0

checkInput :: [String] -> Bool
checkInput list | all (\x -> length x == 1) list = checkInput' list
                | otherwise = False
  where
    checkInput' :: [String] -> Bool
    checkInput' list = length (filter (\x -> elem (head x) numbers) list ) == 6
            where 
              numbers = map intToDigit [0..9]

parseInput :: [String] -> [Pos]
parseInput list = parseInput' (map (\x -> digitToInt (head x)) list)
  where
    parseInput' :: [Int] -> [Pos]
    parseInput' [] = []
    parseInput' (x:y:xs) = (x,y):parseInput' xs

instructions :: IO ()
instructions = do
  putStrLn "Welcome!"
  putStrLn "Insert input with format x y mx my ax ay"
  putStrLn "Where x y is the coordinates of the amazon you wish to move, mx my is the tile you wish to move to and ax ay the coordinates where you wish to fire your arrow."

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
                            | x1 == x2 && y1 == y2 = False
                            | not straightLine = False
                            | otherwise = emptyPath b (x1,y1) (x2,y2)
    where
        --If we aren't moving straight up or down, we have to be moving the same amount of tiles in both x and y direction.
        straightLine = ((x1 == x2 ) || (y1 == y2) ) || (abs (x1 - x2) == abs (y1-y2))
        --Checks if the given path is empty or not
        emptyPath :: Board -> Pos -> Pos -> Bool
        emptyPath b (x1,y1) (x2,y2) | y1 == y2 && east = checkPath b (x1,y1) (x2,y2) 1 0
                                    | y1 == y2 = checkPath b (x1,y1) (x2,y2) (-1) 0
                                    | x1 == x2 && north = checkPath b (x1,y1) (x2,y2) 0 1
                                    | x1 == x2 = checkPath b (x1,y1) (x2,y2) 0 (-1)
                                    | north && east = checkPath b (x1,y1) (x2,y2) 1 1
                                    | east = checkPath b (x1,y1) (x2,y2) 1 (-1)
                                    | north = checkPath b (x1,y1) (x2,y2) (-1) 1
                                    | otherwise = checkPath b (x1,y1) (x2,y2) (-1) (-1)

            where 
                  east = x2 > x1
                  north = y2 > y1
        --Checks if the diagonal path given is empty or not.
        checkPath :: Board -> Pos -> Pos -> Int -> Int -> Bool
        checkPath b (x1,y1) (x2,y2) dx dy | (x1 == x2) && (y1 == y2) = True
                                             | (getPos b ((x1+dx),(y1+dy))) /= Empty = False
                                             | otherwise = checkPath b ((x1+dx),(y1+dy)) (x2,y2) dx dy


-- y2 = all (==Empty) (take (x2-x1) (drop (x1+1) row))
-- all (==Empty) (take (y2-y1) (drop (y1+1) row2))

--Checks if the given position is valid (inside the board)
validPos :: Pos -> Bool
validPos (x,y) = (x >= 0 && x <=9) && (y >= 0 && y <=9)

--Checks if the game is over and returns the color of the victor, empty if not over.
--TODO: Returning a tile is ugly. Perhaps return (Bool, Maybe tile)?
gameOver :: Board -> Tile
gameOver b | overFor b White = White
           | overFor b Black = Black
           | otherwise = Empty
    where
        overFor :: Board -> Tile -> Bool
        overFor b t = all (/=Empty) (concat (map (\x -> tilesAround b x) (findTiles b t)))

tilesAround :: Board -> Pos -> [Tile]
tilesAround b (x,y) | x == 0 && y == 0 = [se,s,e]
                            | x == 9 && y == 9 = [nw,n,w]
                            | x == 0 = [ne,se,n,s,e]
                            | x == 9 = [nw,sw,n,s,w]
                            | y == 0 = [se,sw,s,e,w]
                            | y == 9 = [nw,ne,n,e,w]
                            | otherwise = [nw,ne,se,sw,n,s,e,w]

            where 
                nw = getPos b ((x-1),(y-1))
                ne = getPos b ((x+1),(y-1))
                se = getPos b ((x+1),(y+1))
                sw = getPos b ((x-1),(y+1))
                n = getPos b (x,(y-1))
                s = getPos b (x,(y+1))
                e = getPos b ((x+1),y)
                w = getPos b ((x+1),y)


--Returns the tile at the given position after checking that it is valid.
getPos :: Board -> Pos -> Tile
getPos b (x,y) | validPos (x,y) = ((rows b)!!y)!!x
               | otherwise = error "Invalid position"

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
printBoard b = putStrLn (boardToString (rows b))

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

addCoordinates :: [String] -> [String]
addCoordinates list = ("  "++coordinates)
                      :transpose((" "++coordinates)
                      :transpose((" "++whitespace)
                      :transpose(whitespace
                      :(transpose list))))
  where coordinates = "0123456789"
        whitespace = "          "

