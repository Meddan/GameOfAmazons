import System.Console.ANSI hiding (Black, White)
import System.IO
import Control.Monad
import Data.List.Split
import Data.Char
import Control.Concurrent
import Logic
import Data.Maybe



main :: IO ()
main = do 
  setTitle "Game of Amazons"
  putStrLn "Press i and then enter for instructions or anything else for the game"

  char <- getLine
  if char == "i"
    then resetScreen >> instructions
    else resetScreen >> gameLoop White initialBoard
{-
  The main loop for the game.
  The loop works as follows:
  Print the gameBoard
  Prompt the player for a move.
  Validate the move.
  Determine if the game is over.
  If the move is valid and the game 
  isn't over it passes the turn and repeats for the other player.
-}
gameLoop :: Tile -> Board -> IO ()
gameLoop t b = do 
  resetScreen
  printBoard b
  putStrLn ("It is " ++ show(t) ++ "'s turn")
  line <- getLine
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
              if (gameOver finalBoard ) == Black
                then (putStrLn "Black won!")
                else if (gameOver finalBoard) == White
                then (putStrLn "White won!")
                else do
                  gameLoop (switchTile t) finalBoard
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

-- Validates a given move.
validateMove :: Board -> Tile -> Pos -> Pos -> Bool
validateMove b t p m | fromJust (getPos b p ) /= t = False
                     | otherwise = clearPath b p m

-- Clears the screen so we don't get multiple boards on the screen.
resetScreen :: IO ()
resetScreen = clearScreen >> setSGR [Reset] >> setCursorPosition 0 0

-- Checks if the given input is formatted correctly
checkInput :: [String] -> Bool
checkInput list | all (\x -> length x == 1) list = checkInput' list
                | otherwise = False
  where
    checkInput' :: [String] -> Bool
    checkInput' list = length (filter (\x -> elem (head x) numbers) list ) == 6
            where 
              numbers = map intToDigit [0..9]

-- Takes the input and converts it into a list of positions.
parseInput :: [String] -> [Pos]
parseInput list = parseInput' (map (\x -> digitToInt (head x)) list)
  where
    parseInput' :: [Int] -> [Pos]
    parseInput' [] = []
    parseInput' (x:y:xs) = (x,y):parseInput' xs

-- Prints instructions for the game.
instructions :: IO ()
instructions = do
  putStrLn "Welcome!"
  putStrLn "Insert input with format x y mx my ax ay"
  putStrLn "Where x y is the coordinates of the amazon you wish to move"
  putStrLn "mx my is the tile you wish to move to"
  putStrLn "ax ay the coordinates where you wish to fire your arrow."
  putStrLn "Press any key to start a game"
  anyKey <- getLine
  resetScreen >> gameLoop White initialBoard

-- Switches the turn.
switchTile :: Tile -> Tile
switchTile t | t == White = Black
             | t == Black = White